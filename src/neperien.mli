
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

val send : t -> ?causes:id list -> string -> id
(** New id, with an informal description (the string parameter). It depends
    on some previous ids (the [causes] list), and the current context,
    see {!within} *)

val send_b : t -> ?causes:id list ->
             ('a, Buffer.t, unit, id) format4 -> 'a
(** Same as {!make}, but allows to use Buffer printers to build the
    description. *)

val within : t -> ?causes:id list -> string -> (unit -> 'a) -> 'a * id
(** [within log ?causes msg f] creates a new scope in which events will
    be children of [e], where [e], an event made out of [causes] and [msg]
    is eventually returned. The value returned by [f unit], paired with [e],
    or exception raised by [f unit], is the result of the expression. *)

val within_b : t -> ?causes:id list ->
              ('a, Buffer.t, unit, ((unit -> 'b) -> 'b * id)) format4 -> 'a

(** {2 Log to a File} *)

type encoding =
  | Bencode

val log_to_file : ?encoding:encoding -> string -> t or_error
(** Open the given file in write mode, and in case of success,
    returns a logger that will write every event to the file
    @param encoding how to encode events (default [Bencode]) *)

val log_to_file_exn : ?encoding:encoding -> string -> t
(** Unsafe version of {!log_to_file}.
    @raise Failure if it can't open the file *)

val close : t -> unit
(** Close log. It will not be usable anymore afterwards *)

(** {2 Unsafe}

Some operators that need to be handled with care *)

module Unsafe : sig
  type level

  val within_enter : t -> level
  (** Enter a "within" context, same as {!within}. {b Note}: careful,
      if you forget to call {!within_exit} (especially in case of exception)
      it could mess up the hierarchy. *)

  val within_exit : t -> level -> ?causes:id list -> string -> id
  (** Exit the "within" context, returning the ID of the corresponding event.
      @raise Failure if the level doesn't match *)

  val within_exit_b : t -> level -> ?causes:id list ->
                        ('a, Buffer.t, unit, id) format4 -> 'a
end

(** {2 Module Interface} *)

module type S = Neperien_intf.S

module Make(F : sig val log : t end) : S
module MakeFile(F : sig val filename : string end) : S

val log_to_file_mod : string -> (module S) or_error
(** First-class module variant of {!log_to_file} *)
