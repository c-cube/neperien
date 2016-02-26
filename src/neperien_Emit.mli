
(* This file is free software, part of Neperien. See file "LICENSE" for more details. *)

(** {1 Causal Graph} for Debugging
    As often, for unique name generation reasons, this module is not thread
    safe (several causes may have the same name otherwise, which can break
    serialization).
    Causal loops should be avoided.
*)

type 'a sequence = ('a -> unit) -> unit
type 'a or_error = [`Ok of 'a | `Error of string ]
(** Structural types. *)

(** {2 Loggers} *)

type t
(** A logger. It contains a graph of events. *)

val log_to_file : string -> t or_error
(** Open the given file in write mode, and in case of success,
    returns a logger that will write every event to the file *)

val log_to_file_exn : string -> t
(** Unsafe version of {!log_to_file}.
    @raise Failure if it can't open the file *)

val close : t -> unit
(** Close log. It will not be usable anymore afterwards *)


(** {2 Basic Causal Description} *)

val send : t -> ?causes:Neperien_Event.id list -> string -> Neperien_Event.id
(** New id, with an informal description (the string parameter). It depends
    on some previous ids (the [causes] list), and the current context,
    see {!within} *)

val send_b : t -> ?causes:Neperien_Event.id list ->
             ('a, Buffer.t, unit, Neperien_Event.id) format4 -> 'a
(** Same as {!send}, but allows to use Buffer printers to build the
    description. *)

val within : t -> ?causes:Neperien_Event.id list -> string -> (unit -> 'a) -> 'a * Neperien_Event.id
(** [within log ?causes msg f] creates a new scope in which events will
    be children of [e], where [e], an event made out of [causes] and [msg]
    is eventually returned. The value returned by [f ()], paired with [e]'s id,
    or exception raised by [f ()], is the result of the expression. *)

val within_b : t -> ?causes:Neperien_Event.id list ->
              ('a, Buffer.t, unit, ((unit -> 'b) -> 'b * Neperien_Event.id)) format4 -> 'a
(** Same as {!within} but allows to use Buffer printers to build the
    description. *)

val send' : t -> ?causes:Neperien_Event.id list -> string -> unit
val send_b' : t -> ?causes:Neperien_Event.id list -> ('a, Buffer.t, unit, unit) format4 -> 'a
val within' : t -> ?causes:Neperien_Event.id list -> string -> (unit -> 'a) -> 'a
val within_b' : t -> ?causes:Neperien_Event.id list ->
  ('a, Buffer.t, unit, ((unit -> 'b) -> 'b)) format4 -> 'a
(** Same as previous functions, but do not return the log event id. *)


(** {2 Unsafe}
    Some operators that need to be handled with care *)

type level = int

module Unsafe : sig
  val within_enter : t -> level
  (** Enter a "within" context, same as {!within}. {b Note}: careful,
      if you forget to call {!within_exit} (especially in case of exception)
      it could mess up the hierarchy. *)

  val within_exit : t -> level -> ?causes:Neperien_Event.id list -> string -> Neperien_Event.id
  (** Exit the "within" context, returning the ID of the corresponding event.
      @raise Failure if the level doesn't match *)

  val within_exit_b : t -> level -> ?causes:Neperien_Event.id list ->
                        ('a, Buffer.t, unit, Neperien_Event.id) format4 -> 'a

  val within_exit' : t -> level -> ?causes:Neperien_Event.id list -> string -> unit
  val within_exit_b' : t -> level -> ?causes:Neperien_Event.id list ->
                        ('a, Buffer.t, unit, unit) format4 -> 'a
end

