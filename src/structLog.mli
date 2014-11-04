
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

(** {1 Causal Graph} for Debugging
As often, for unique name generation reasons, this module is not thread
safe (several causes may have the same name otherwise, which can break
serialization).
Causal loops should be avoided. *)

type t
(** A logger. It contains a graph of events. *)

type 'a sequence = ('a -> unit) -> unit
type 'a or_error = [`Ok of 'a | `Error of string ]

(** {2 Basic Causal Description} *)

type id
(** Unique ID for an event, past or current *)

val make : t -> ?within:id -> ?causes:id list -> string -> id
(** New id, with an informal description (the string parameter). It depends
    on some previous ids (the [causes] list), and some more global context
    (ongoing event/task, see [within]). *)

val make_b : t -> ?within:id -> ?causes:id list ->
             ('a, Buffer.t, unit, id) format4 -> 'a
(** Same as {!make}, but allows to use Buffer printers to build the
    description. *)

(** {2 Log to a File} *)

val log_to_file : string -> t or_error
(** Open the given file in write mode, and in case of success,
    returns a logger that will write every event to the file *)

val log_to_file_exn : string -> t
(** Unsafe version of {!log_to_file}.
    @raise Failure if it can't open the file *)

val root : t -> id
(** Root event (the start of the program?) *)

val close : t -> unit
(** Close log. It will not be usable anymore afterwards *)
