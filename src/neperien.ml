
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

type t = {
  mutable stack : stack_cell list;
  mutable stack_len : int;
  get_id : unit -> id; (* fresh ID *)
  on_event : event -> unit;
  on_close : unit -> unit;
}

(** {2 Basic Causal Description} *)

let _prev t = match t.stack with
  | [] -> None
  | sc :: _ -> sc.cur_child

let _make t causes descr last_child =
  let id = t.get_id() in
  let level = t.stack_len + 1 in
  (* maintain the prev/child pointers *)
  let prev = _prev t in
  begin match t.stack with
    | [] -> ()
    | sc :: _ -> sc.cur_child <- Some id
  end;
  { id; descr; causes; prev; level; last_child }

let make t ?(causes=[]) descr =
  let e = _make t causes descr None in
  t.on_event e;
  e.id

let make_b t ?causes fmt =
  let buf = Buffer.create 24 in
  Printf.kbprintf
    (fun buf -> make t ?causes (Buffer.contents buf))
    buf fmt

let _push t =
  let level = t.stack_len + 1 in
  t.stack_len <- level;
  t.stack <- {sc_prev=_prev t; cur_child=None; } :: t.stack;
  level

let _pop t level =
  if level != t.stack_len
    then failwith "pop: mismatched levels";
  match t.stack with
  | [] -> assert false
  | sc :: tail ->
      t.stack <- tail;
      t.stack_len <- t.stack_len-1;
      sc

let within t ?(causes=[]) msg f =
  try
    let level = _push t in
    let x = f () in
    let sc = _pop t level in
    (* create event, pointing to its last child, if any *)
    let last_child = sc.cur_child in
    let e = _make t causes msg last_child in
    (*  emit now, the event is complete *)
    t.on_event e;
    x, e.id
  with ex ->
    t.stack <- List.tl t.stack;
    raise ex

let within_b t ?causes fmt =
  let buf = Buffer.create 24 in
  Printf.kbprintf
    (fun buf -> fun f -> within t ?causes (Buffer.contents buf) f)
    buf fmt

(** {2 Unsafe} *)

module Unsafe = struct
  type level = int

  let within_enter t = _push t

  let within_exit t lev ?(causes=[]) msg =
    let sc = _pop t lev in
    (* create event, pointing to its last child, if any *)
    let last_child = sc.cur_child in
    let e = _make t causes msg last_child in
    (*  emit now, the event is complete *)
    t.on_event e;
    e.id

  let within_exit_b t level ?causes fmt =
    let buf = Buffer.create 24 in
    Printf.kbprintf
      (fun buf -> within_exit t level ?causes (Buffer.contents buf))
      buf fmt
end

(** {2 Encoding to file} *)

type encoding =
  | Bencode

let _pp_id_list oc l =
  output_char oc 'l';
  List.iter (fun id -> Printf.fprintf oc "i%de" id) l;
  output_char oc 'e';
  ()

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

(** {2 Log to a File} *)

let _version_string = "i1e\n" (* version 1 *)

let log_to_file ?(encoding=Bencode) filename =
  try
    let oc = match encoding with
      | Bencode -> open_out filename
    in
    let on_event = match encoding with
      | Bencode -> (fun e -> _encode_bencode oc e)
    in
    let get_id() = pos_out oc in
    let on_close _ = flush oc; close_out_noerr oc in
    output_string oc _version_string; (* version string = header *)
    let t = { stack=[]; stack_len=0; get_id; on_event; on_close; } in
    Gc.finalise on_close t;
    `Ok t
  with e ->
    `Error (Printexc.to_string e)

let log_to_file_exn ?encoding filename =
  match log_to_file ?encoding filename with
   | `Error msg -> failwith msg
   | `Ok x -> x

let close t = t.on_close ()

(** {2 Module Interface} *)

module type S = sig
  val make : ?causes:id list -> string -> id
  (** New id, with an informal description (the string parameter). It depends
      on some previous ids (the [causes] list), and some more global context
      (ongoing event/task, see [within]). *)

  val make_b : ?causes:id list ->
               ('a, Buffer.t, unit, id) format4 -> 'a
  (** Same as {!make}, but allows to use Buffer printers to build the
      description. *)
end

let log_to_file_mod filename =
  match log_to_file filename with
  | `Ok x ->
      let module M = struct
        let make ?causes msg = make x ?causes msg
        let make_b ?causes msg = make_b x ?causes msg
      end in
      `Ok (module M : S)
  | `Error e -> `Error e
