
(* This file is free software, part of Neperien. See file "LICENSE" for more details. *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a or_error = [`Ok of 'a | `Error of string]

module B = Bencode_types
module E = Neperien_Event
module H = Neperien_Header

(*
   ### Log Parsers
   Log parsers are handle on log files. They contain information
   on their current position in the file, and reading from them
   will affect that position.
   A copy function is thus given, returning a copy of the handle,
   with a fresh starting position (at the beginning of the file).
*)

type t = {
  chan : in_channel;
  lexbuf : Lexing.lexbuf;
  filename : string;
}

let close log = close_in log.chan

let open_file_exn filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  (* close file on GC *)
  let log = { chan; lexbuf; filename } in
  Gc.finalise close log;
  log

let open_file filename =
  try `Ok (open_file_exn filename)
  with e -> `Error (Printexc.to_string e)

let fresh t =
  let chan = open_in t.filename in
  let lexbuf = Lexing.from_channel chan in
  { t with chan; lexbuf; }

(*
   ### Parsing
   Events can be accessed directly by id (i.e offset in a file).
*)

(* Parse the next bencode value *)
let parse_next_bencode t =
  Bencode_parse.bencode Bencode_lex.bencode t.lexbuf

(* Parse a bencode value at offset i in file t.chan *)
let parse_bencode t i =
  seek_in t.chan i;
  Lexing.flush_input t.lexbuf;
  parse_next_bencode t

(* Parse the next log event *)
let parse_next t = E.decode @@ parse_next_bencode t

let parse_header t = H.decode @@ parse_next_bencode t

(* Get events by id *)
let by_id_exn log id =
  let b = parse_bencode log id in
  E.decode b

let by_id log id =
  try Some (by_id_exn log id)
  with Failure _ -> None

(*
   ### Iterators
   Give iterators to more easily access bundle of events
*)

let iter t k =
  (* duplicate log reader *)
  let log = fresh t in
  try
    (* read header *)
    let _ = parse_header log in
    (* traverse *)
    while true do
      k @@ parse_next log
    done
  with
  | Parsing.Parse_error ->
    close_in log.chan; () (* end of file.. *)
  | e ->
    close_in log.chan; raise e

let iter_below log ~level k =
  assert (level >= 0);
  iter log (fun e -> if e.E.level <= level then k e)

(*
   NOTE: I'm really suspect of the actual use of this function, given that
   I can't find any semantic (in terms of the logging hierachy) to give it...

let iter_from log e k =
  (* Positioning at e.id, We throw the value away. *)
  let _ = parse_bencode log e.E.id in
  try while true do
      k @@ parse_next log
    done
  with Parsing.Parse_error -> ()
*)

let rec iter_prev log e k =
  match e.E.prev with
  | None -> ()
  | Some id ->
    let b = parse_bencode log id in
    let e' = E.decode b in
    k e';
    iter_prev log e' k

let iter_children log e k =
  match e.E.last_child with
  | None -> ()
  | Some id ->
    let b = parse_bencode log id in
    let e' = E.decode b in
    k e';
    iter_prev log e' k

(*
   ### Generators
   Give generators to more easily access bundle of events
*)

let gen t =
  (* duplicate log reader *)
  let log = fresh t in
  (* read header *)
  let _ = parse_header log in
  let stop = ref false in
  fun () ->
    if !stop then None
    else try
        Some (parse_next log)
      with
      | Parsing.Parse_error ->
        close_in log.chan;
        stop := true;
        None (* end of file.. *)
      | e ->
        close_in log.chan;
        raise e

let gen_below log ~level =
  let g = gen log in
  let rec next_ () =
    match g() with
    | Some e as res when e.E.level <= level -> res
    | Some _ -> next_()
    | None -> None
  in next_

(*
   Same as iter_from, :p

let gen_from log e =
  let _ = parse_bencode log e.E.id in
  let stop = ref false in
  fun () ->
    if !stop then None
    else try
        let b = parse_next_bencode_ log in
        let e = E.decode b in
        Some e
      with Parsing.Parse_error ->
        stop := true; None
*)

let gen_prev log e =
  let cur = ref e in
  fun () -> match (!cur).E.prev with
    | None -> None
    | Some id ->
      let e' = parse_next log in
      cur := e';
      Some e'

let gen_children log e =
  match e.E.last_child with
  | None -> (fun () -> None)
  | Some id ->
    let e' = parse_next log in
    let first = ref true in
    let tail = gen_prev log e' in
    fun () ->
      if !first then begin
        first := false;
        Some e'
      end else
        tail ()


