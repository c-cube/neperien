
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

type id

val equal_id : id -> id -> bool
val string_of_id : id -> string

type event = {
  id : id;
  descr : string;
  causes : id list;
  level : int;            (* depth in tree *)
  prev : id option;       (* previous node in tree *)
  last_child: id option;  (* ID of last child, if any *)
} (** Node of the on-disk tree *)

type t
(** A log-file parser, giving access to a tree of {!event} nodes *)

(** {2 Access} *)

val by_id : t -> id -> event option
(** Access an event by its ID within the file. Performs IO. *)

val by_id_exn : t -> id -> event
(** @raise Failure if the record can't be found *)

val iter : t -> event sequence
(** Linear sequence of events, by increasing ID *)

val iter_below : t -> level:int -> event sequence
(** [iter_above log ~level] iterates only on events whose level is [<= level],
    jumping over more detailed logs. *)

val iter_children : t -> event -> event sequence
(** Iterate on direct children of the given event, in {b reverse} order (last
    child first). *)

(** {2 Open file} *)

val open_file : string -> t or_error
(** Open the given file, expecting it to contain serialized logs *)

val open_file_exn : string -> t
(** @raise Failure if it can't open the file *)

val close : t -> unit
(** Close the underlying file. From now on operations on the log will fail.
    Performed automatically if the log is GC'd *)
