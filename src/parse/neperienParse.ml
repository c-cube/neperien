
(*
copyright (c) 2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Read a Log file} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a or_error = [`Ok of 'a | `Error of string]

(** {2 Main Types} *)

type id = int
(* offset within file *)

let equal_id (i:id) j = i=j
let string_of_id = string_of_int
let pp_id buf = Printf.bprintf buf "%d"

type event = {
  id : id;
  descr : string;
  causes : id list;
  level : int;            (* depth in tree *)
  prev : id option;       (* previous node in tree *)
  last_child: id option;  (* ID of last child, if any *)
}
(** Node of the on-disk tree *)

module B = Bencode_types

type t = {
  chan : in_channel;
  lexbuf : Lexing.lexbuf;
  filename : string;
}

(* parse a bencode value at offset i in file t.chan *)
let parse_bencode_ t i =
  seek_in t.chan i;
  Lexing.flush_input t.lexbuf;
  Bencode_parse.bencode Bencode_lex.bencode t.lexbuf

let parse_next_bencode_ t =
  Bencode_parse.bencode Bencode_lex.bencode t.lexbuf

let _unwrap_exn msg o = match o with
  | None -> failwith msg
  | Some x -> x

let get_field name b =
  try List.assoc name b
  with Not_found -> failwith ("expected field " ^ name)
let get_int b = match b with
  | B.Integer i -> i
  | _ -> failwith "expected int"
let get_str b = match b with
  | B.String s -> s
  | _ -> failwith "expected string"
let get_l get b = match b with
  | B.List l' -> List.map get l'
  | _ -> failwith "expected list"

let ev_of_bencode_ b = match b with
  | B.Dict l ->
      let id = get_int (get_field "i" l) in
      let descr = get_str (get_field "d" l) in
      let causes =
        try get_l get_int (List.assoc "c" l)
        with Not_found -> []
      in
      let level = get_int (get_field "l" l) in
      let prev = try Some (get_int (List.assoc "p" l)) with Not_found -> None in
      let last_child = try Some(get_int (List.assoc "lc" l)) with Not_found -> None in
      { id; descr; causes; level; prev; last_child; }
  | _ -> failwith "expected dict"

let by_id_exn log id =
  let b = parse_bencode_ log id in
  ev_of_bencode_ b

let by_id log id =
  try Some (by_id_exn log id)
  with Failure _ -> None

let iter log k =
  (* duplicate log reader *)
  let chan = open_in log.filename in
  let lexbuf = Lexing.from_channel chan in
  let log' = {log with chan; lexbuf; } in
  try
    (* read header *)
    let b = parse_next_bencode_ log' in
    begin match b with
      | B.Integer 1 -> ()
      | B.Integer v -> failwith ("unknown version: " ^ string_of_int v)
      | _ -> failwith "expected version header"
    end;
    (* traverse *)
    while true do
      let b = parse_next_bencode_ log' in
      let e = ev_of_bencode_ b in
      k e
    done
  with
  | Parsing.Parse_error -> close_in chan; () (* end of file.. *)
  | e -> close_in chan; raise e

let iter_below log ~level k =
  assert (level >= 0);
  iter log (fun e -> if e.level <= level then k e)

let iter_from log e k =
  (* Positioning at e.id, We throw the value away. *)
  let _ = parse_bencode_ log e.id in
  try while true do
    let b = parse_next_bencode_ log in
    let e = ev_of_bencode_ b in
    k e
    done
  with Parsing.Parse_error -> ()

let rec iter_from_prev log e k = match e.prev with
  | None -> ()
  | Some id ->
      let b = parse_bencode_ log id in
      let e' = ev_of_bencode_ b in
      k e';
      iter_from_prev log e' k

let iter_children log e k = match e.last_child with
  | None -> ()
  | Some id ->
      let b = parse_bencode_ log id in
      let e' = ev_of_bencode_ b in
      k e';
      iter_from_prev log e' k

let gen log =
  (* duplicate log reader *)
  let chan = open_in log.filename in
  let lexbuf = Lexing.from_channel chan in
  let log' = {log with chan; lexbuf; } in
  (* read header *)
  let b = parse_next_bencode_ log' in
  begin match b with
    | B.Integer 1 -> ()
    | B.Integer v -> failwith ("unknown version: " ^ string_of_int v)
    | _ -> failwith "expected version header"
  end;
  let stop = ref false in
  fun () ->
    if !stop then None
    else try
      let b = parse_next_bencode_ log' in
      let e = ev_of_bencode_ b in
      Some e
    with
    | Parsing.Parse_error ->
        close_in chan;
        stop := true;
        None (* end of file.. *)
    | e -> close_in chan; raise e

let gen_below log ~level =
  let g = gen log in
  let rec next_ () =
    match g() with
    | Some e as res when e.level <= level -> res
    | Some _ -> next_()
    | None -> None
  in next_

let gen_from log e =
  let _ = parse_bencode_ log e.id in
  let stop = ref false in
  fun () ->
    if !stop then None
    else try
      let b = parse_next_bencode_ log in
      let e = ev_of_bencode_ b in
      Some e
    with Parsing.Parse_error ->
      stop := true; None

let gen_from_prev log e =
  let cur = ref e in
  fun () -> match (!cur).prev with
    | None -> None
    | Some id ->
      let b = parse_bencode_ log id in
      let e' = ev_of_bencode_ b in
      cur := e';
      Some e'

let gen_children log e = match e.last_child with
  | None -> (fun() -> None)
  | Some id ->
      let b = parse_bencode_ log id in
      let e' = ev_of_bencode_ b in
      let first = ref true in
      let tail = gen_from_prev log e' in
      fun () ->
        if !first
        then (first:=false; Some e')
        else tail()

(** {2 Open file} *)

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
