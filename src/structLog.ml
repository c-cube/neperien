
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

(* one event, before serialization *)
type event = {
  id : id;
  descr : string;
  within : id option;
  causes : id list;
}

type t = {
  mutable cur_id : id;
  stack : id Stack.t;  (* stack of 'within' *)
  on_event : event -> unit;
  on_close : unit -> unit;
}

(** {2 Basic Causal Description} *)

let make t ?(causes=[]) descr =
  let id = t.cur_id in
  t.cur_id <- id + 1;
  let within =
    if Stack.is_empty t.stack then None else Some (Stack.top t.stack)
  in
  t.on_event { id; descr; within; causes; };
  id

let make_b t ?causes fmt =
  let buf = Buffer.create 24 in
  Printf.kbprintf
    (fun buf -> make t ?causes (Buffer.contents buf))
    buf fmt

let within t id f =
  try
    Stack.push id t.stack;
    let x = f () in
    let id2 = Stack.pop t.stack in
    assert (id==id2);
    x
  with e ->
    ignore (Stack.pop t.stack);
    raise e

(** {2 Encoding to file} *)

type encoding =
  | Binary
  | Bencode

(* format: Bencode dictionary
  "c": list int  (causes IDs)
  "d": string (description)
  "i": int (id)
  "w": optional int (within, parent event)
  *)
let _encode_bencode oc e =
  output_string oc "d1:cl";
  List.iter (fun id -> Printf.fprintf oc "i%de" id) e.causes;
  Printf.fprintf oc "e1:d%d:%s" (String.length e.descr) e.descr;
  Printf.fprintf oc "1:ii%de" e.id;
  begin match e.within with
    | None -> ()
    | Some id -> Printf.fprintf oc "1:wi%de" id
  end;
  output_char oc 'e'

(* format:
  id:int          = unique ID
  n:int           = size of list of causes
  int_1,...,int_n = list of causes
  d:int           = size of descr, in bytes
  string          = description (length d)
  w:int           = 0 if no "within" | the ID of context
*)
let _encode_binary oc e =
  assert (e.id > 0);
  output_binary_int oc e.id;
  output_binary_int oc (List.length e.causes);
  List.iter (output_binary_int oc) e.causes;
  output_binary_int oc (String.length e.descr);
  output_string oc e.descr;
  match e.within with
    | None -> output_binary_int oc 0
    | Some i -> assert (i!=0); output_binary_int oc i

(** {2 Log to a File} *)

let log_to_file ?(encoding=Binary) filename =
  try
    let oc = match encoding with
      | Binary -> open_out_bin filename
      | Bencode -> open_out filename
    in
    let on_event = match encoding with
      | Binary -> (fun e -> _encode_binary oc e)
      | Bencode -> (fun e -> _encode_bencode oc e)
    in
    let on_close _ = flush oc; close_out_noerr oc in
    (* NOTE: we start with id=1 to make binary encoding easier *)
    let t = { stack=Stack.create(); cur_id=1; on_event; on_close; } in
    Gc.finalise on_close t;
    `Ok t
  with e ->
    `Error (Printexc.to_string e)

let log_to_file_exn ?encoding filename =
  match log_to_file ?encoding filename with
   | `Error msg -> failwith msg
   | `Ok x -> x

let close t = t.on_close ()

(** {2 Unsafe} *)

module Unsafe = struct
  let within_enter t id = Stack.push id t.stack
  let within_exit t = ignore (Stack.pop t.stack)
end

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
