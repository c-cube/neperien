
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
type 'a or_error = [`Ok of 'a | `Error of string]

(** {2 Main Types} *)

type id = int
(* offset within file *)

let equal_id (i:id) j = i=j
let string_of_id = string_of_int

type event = {
  id : id;
  descr : string;
  causes : id list;
  level : int;            (* depth in tree *)
  prev : id option;       (* previous node in tree *)
  last_child: id option;  (* ID of last child, if any *)
}
(** Node of the on-disk tree *)

module BA = Bigarray
module BA1 = Bigarray.Array1

type maped_file =
  (char, BA.int8_unsigned_elt, BA.c_layout) BA1.t

type t = {
  fd : Unix.file_descr;
  arr : maped_file;
}

(* TODO: parse a Bencode value at an offset in [arr] *)
(* TODO: change the bencode value into an event *)

let by_id_exn log id =
  assert false (* TODO *)

let by_id log id =
  try Some (by_id_exn log id)
  with Failure _ -> None

let iter log k =
  assert false (* TODO *)

let iter_below log ~level k =
  assert (level >= 0);
  iter log (fun e -> if e.level <= level then k e)

let iter_children log e =
  assert false (* TODO *)

(** {2 Open file} *)

let close log =
  Unix.close log.fd

let open_file_exn filename =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
  (* map to an array *)
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let _ = Unix.lseek fd 0 Unix.SEEK_SET in
  let arr = BA1.map_file fd BA.char BA.C_layout false len in
  (* close file on GC *)
  let log = { fd; arr; } in
  Gc.finalise close log;
  log

let open_file filename =
  try `Ok (open_file_exn filename)
  with e -> `Error (Printexc.to_string e)
