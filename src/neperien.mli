
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

val make : t -> ?causes:id list -> string -> id
(** New id, with an informal description (the string parameter). It depends
    on some previous ids (the [causes] list), and some more global context
    (ongoing event/task, see [within]).
    
    @param within *)

val make_b : t -> ?causes:id list ->
             ('a, Buffer.t, unit, id) format4 -> 'a
(** Same as {!make}, but allows to use Buffer printers to build the
    description. *)

val within : t -> ?causes:id list -> string -> (id -> 'a) -> 'a
(** [within log ?causes msg f] makes an event out of [causes] and [msg],
    calls it [id], and runs [f id] in a context in which any event
    will be considered as "within" [id]. The value returned by [f id],
    or exception raised by [f id], is the result of the expression. *)

val within_b : t -> ?causes:id list ->
              ('a, Buffer.t, unit, ((id -> 'b) -> 'b)) format4 -> 'a

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
  val within_enter : t -> ?causes:id list -> string -> id
  (** Enter a "within" context, same as {!within}. {b Note}: careful,
      if you forget to call {!within_exit} (especially in case of exception)
      it could mess up the hierarchy. *)

  val within_enter_b : t -> ?causes:id list ->
                        ('a, Buffer.t, unit, id) format4 -> 'a

  val within_exit : t -> id -> unit
  (** Exit the "within" context.
      @raise Failure if the ID doesn't match the current level (programmer error) *)
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

val log_to_file_mod : string -> (module S) or_error
(** First-class module variant of {!log_to_file} *)
