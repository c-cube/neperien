
(*
copyright (c) 2013-2014, simon cruanes
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

(** {1 Causal Graph} for Debugging *)

type 'a sequence = ('a -> unit) -> unit
type 'a or_error = [`Ok of 'a | `Error of string ]

type id = int
(** Offset in file *)

(** {2 Basic Causal Description} *)

(* one event, before serialization *)
type event = {
  id : id;
  descr : string;
  causes : id list;
  level : int;
  prev : id option;
  last_child : id option;
}

type stack_cell = {
  mutable cur_child : id option;
  sc_prev : id option;
}

(** {2 Encoding to file} *)

let _pp_id_list oc l =
  output_char oc 'l';
  List.iter (fun id -> Printf.fprintf oc "i%de" id) l;
  output_char oc 'e';
  ()

(* format version *)
let _version_string = "i1e\n"

(* format: Bencode dictionary
  "c": list int  (causes IDs)
  "d": string (description)
  "i": int (id)
  "l": int (level)
  "lc": int option (last child ID)
  "p": int option (previous ID)
  *)
let _encode_bencode oc e =
  output_char oc 'd';
  (* causes *)
  output_string oc "1:c";
  _pp_id_list oc e.causes;
  (* descr *)
  Printf.fprintf oc "1:d%d:%s" (String.length e.descr) e.descr;
  (* ID *)
  Printf.fprintf oc "1:ii%de" e.id;
  (* level *)
  Printf.fprintf oc "1:li%de" e.level;
  (* last child *)
  begin match e.last_child with
    | None -> ()
    | Some i -> Printf.fprintf oc "2:lci%de" i
  end;
  (* previous *)
  begin match e.prev with
    | None -> ()
    | Some i -> Printf.fprintf oc "1:pi%de" i
  end;
  (* end *)
  output_string oc "e\n";
  ()

(** {2 Module Interface} *)

module type S = Neperien_intf.S

module MakeFile(F : sig
  val filename : string
end) = struct
  (* open file *)
  let oc = open_out_gen [Open_append; Open_creat; Open_trunc] 0o644 F.filename

  let stack : stack_cell list ref = ref []
  let stack_len = ref 0
  let get_id () = pos_out oc
  let _close oc = 
    flush oc;
    close_out_noerr oc
  let close() = _close oc
  let emit_event e = _encode_bencode oc e

  let _prev () = match !stack with
    | [] -> None
    | sc :: _ -> sc.cur_child

  let _make causes descr last_child =
    let id = get_id() in
    let level = !stack_len + 1 in
    (* maintain the prev/child pointers *)
    let prev = _prev () in
    begin match !stack with
      | [] -> ()
      | sc :: _ -> sc.cur_child <- Some id
    end;
    { id; descr; causes; prev; level; last_child }

  let send ?(causes=[]) descr =
    let e = _make causes descr None in
    emit_event e;
    e.id

  let send_b ?causes fmt =
    let buf = Buffer.create 24 in
    Printf.kbprintf
      (fun buf -> send ?causes (Buffer.contents buf))
      buf fmt

  let _push () =
    let level = !stack_len + 1 in
    stack_len := level;
    stack := {sc_prev=_prev (); cur_child=None; } :: !stack;
    level

  let _pop level =
    if level != !stack_len
      then failwith "pop: mismatched levels";
    match !stack with
    | [] -> assert false
    | sc :: tail ->
        stack := tail;
        stack_len := !stack_len-1;
        sc

  let within ?(causes=[]) msg f =
    let level = _push () in
    try
      let x = f () in
      let sc = _pop level in
      (* create event, pointing to its last child, if any *)
      let last_child = sc.cur_child in
      let e = _make causes msg last_child in
      (*  emit now, the event is complete *)
      emit_event e;
      x, e.id
    with ex ->
      ignore (_pop level);
      raise ex

  let within_b ?causes fmt =
    let buf = Buffer.create 24 in
    Printf.kbprintf
      (fun buf -> fun f -> within ?causes (Buffer.contents buf) f)
      buf fmt

  let send' ?causes msg = ignore(send ?causes msg)
  let send_b' ?causes fmt = 
    let buf = Buffer.create 24 in
    Printf.kbprintf
      (fun buf -> ignore(send ?causes (Buffer.contents buf)))
      buf fmt
  let within' ?causes msg f =
    fst (within ?causes msg f)
  let within_b' ?causes fmt =
    let buf = Buffer.create 24 in
    Printf.kbprintf
      (fun buf -> fun f -> fst(within ?causes (Buffer.contents buf) f))
      buf fmt

  module Unsafe = struct
    let within_enter () = _push ()

    let within_exit lev ?(causes=[]) msg =
      let sc = _pop lev in
      (* create event, pointing to its last child, if any *)
      let last_child = sc.cur_child in
      let e = _make causes msg last_child in
      (*  emit now, the event is complete *)
      emit_event e;
      e.id

    let within_exit_b level ?causes fmt =
      let buf = Buffer.create 24 in
      Printf.kbprintf
        (fun buf -> within_exit level ?causes (Buffer.contents buf))
        buf fmt

    let within_exit' lev ?causes msg =
      ignore (within_exit lev ?causes msg)
    let within_exit_b' lev ?causes fmt =
      let buf = Buffer.create 24 in
      Printf.kbprintf
        (fun buf -> ignore(within_exit lev ?causes (Buffer.contents buf)))
        buf fmt
  end

  (* init code *)
  let () =
    Gc.finalise _close oc;
    output_string oc _version_string; (* version string = header *)
    ()
end

module Dummy = struct
  let close() = ()
  let _id = 1
  let _buf = Buffer.create 1 (* dummy *)

  (* safety checks *)
  let _level = ref 0

  let _push () =
    incr _level;
    !_level

  let _pop lev =
    if lev <> !_level
      then failwith "pop: mismatched levels";
    decr _level

  let send ?causes s = _id

  let send_b ?causes fmt =
    Buffer.clear _buf;
    Printf.kbprintf (fun _ -> _id) _buf fmt

  let within ?causes s f = f (), _id

  let within_b ?causes fmt =
    Buffer.clear _buf;
    Printf.kbprintf (fun _ -> fun f -> f (), _id) _buf fmt

  let send' ?causes s = ()
  let send_b' ?causes fmt =
    Printf.ifprintf _buf fmt

  let within' ?causes msg f = fst (within ?causes msg f)
  let within_b' ?causes fmt =
    Buffer.clear _buf;
    Printf.kbprintf (fun _ -> fun f -> f ()) _buf fmt

  module Unsafe = struct
    let within_enter = _push

    let within_exit lev ?causes msg = _pop lev; _id

    let within_exit_b lev ?causes fmt =
      Buffer.clear _buf;
      Printf.kbprintf (fun _ -> _pop lev; _id) _buf fmt

    let within_exit' lev ?causes msg = _pop lev
    let within_exit_b' lev ?causes fmt =
      Buffer.clear _buf;
      Printf.kbprintf (fun _ -> _pop lev) _buf fmt
  end
end

let log_to_file_mod filename =
  try
    let module M = MakeFile(struct let filename=filename end) in
    `Ok (module M : S)
  with e ->
    `Error (Printexc.to_string e)

type t = (module S)

let send (module L:S) ?causes msg = L.send ?causes msg
let send_b (module L:S) ?causes msg = L.send_b ?causes msg
let within (module L:S) ?causes msg f = L.within ?causes msg f
let within_b (module L:S) ?causes fmt = L.within_b ?causes fmt

let send' (module L:S) ?causes msg = L.send' ?causes msg
let send_b' (module L:S) ?causes msg = L.send_b' ?causes msg
let within' (module L:S) ?causes msg f = L.within' ?causes msg f
let within_b' (module L:S) ?causes fmt = L.within_b' ?causes fmt

let close (module L:S) = L.close ()

type level = Neperien_intf.level

module Unsafe = struct
  let within_enter (module L:S) =
    L.Unsafe.within_enter ()
  let within_exit (module L:S) level ?causes msg =
    L.Unsafe.within_exit level ?causes msg
  let within_exit_b (module L:S) level ?causes fmt =
    L.Unsafe.within_exit_b level ?causes fmt

  let within_exit' (module L:S) level ?causes msg =
    L.Unsafe.within_exit' level ?causes msg
  let within_exit_b' (module L:S) level ?causes fmt =
    L.Unsafe.within_exit_b' level ?causes fmt
end

(** {2 Log to a File} *)

let log_to_file filename =
  log_to_file_mod filename

let log_to_file_exn filename = match log_to_file filename with
   | `Error msg -> failwith msg
   | `Ok x -> x

let log_none = (module Dummy : S)
