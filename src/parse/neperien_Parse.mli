
(* This file is free software, part of Neperien. See file "LICENSE" for more details. *)

(** {1 Structural types used} *)

type 'a sequence = ('a -> unit) -> unit
type 'a gen = unit -> 'a option
type 'a or_error = [`Ok of 'a | `Error of string]

(** {2 Log parser} *)

type t
(** A log-file parser, giving access to a tree of {!event} nodes *)

val open_file : string -> t or_error
(** Open the given file, expecting it to contain serialized logs *)

val open_file_exn : string -> t
(** @raise Failure if it can't open the file *)

val close : t -> unit
(** Close the underlying file. From now on operations on the log will fail.
    Performed automatically if the log is GC'd *)

(** {2 Direct Access} *)

val root : t -> Neperien_Event.t option
(** Returns the root of a log file. *)

val root_exn : t -> Neperien_Event.t
(* @raise Failure if the root can't be found *)

val by_id : t -> Neperien_Event.id -> Neperien_Event.t option
(** Access an event by its ID within the file. Performs IO. *)

val by_id_exn : t -> Neperien_Event.id -> Neperien_Event.t
(** @raise Failure if the record can't be found *)

(** {2 Iterators over events } *)

val iter : t -> Neperien_Event.t sequence
(** Linear sequence of events, by increasing ID.
    WARNING: There is absolutely *NO* relation between the IDs
             and the order of emission of events. *)

val iter_below : t -> level:int -> Neperien_Event.t sequence
(** [iter_above log ~level] iterates only on events whose level is [<= level],
    jumping over more detailed logs. *)

val iter_prev : t -> Neperien_Event.t -> Neperien_Event.t sequence
(** [iter_prev log e] iterates event in reversed order starting
    from [e], excluded. It only iterates on the same level. *)

val iter_children : t -> Neperien_Event.t -> Neperien_Event.t sequence
(** Iterate on direct children of the given event, in {b reverse} order
    of emission (i.e last child first). *)

(** {2 Generators of events} *)

val gen : t -> Neperien_Event.t gen
(** See {!iter} *)

val gen_below : t -> level:int -> Neperien_Event.t gen
(** Events whose level is <= to the given one *)

val gen_prev : t -> Neperien_Event.t -> Neperien_Event.t gen
(** See {!iter_from_prev} *)

val gen_children : t -> Neperien_Event.t -> Neperien_Event.t gen
(** See {!iter_children} *)

