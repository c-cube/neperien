
(* This file is free software, part of Neperien. See file "LICENSE" for more details. *)

(** {2 Log Events} *)

type id = int

type t = {
  id : id;
  descr : string;
  causes : id list;
  level : int;            (* depth in tree *)
  prev : id option;       (* previous node in tree *)
  last_child: id option;  (* ID of last child, if any *)
}
(** The type for events. An event is indexed by its id, which is its offset
    in the log file.
    TODO: describe how the events are linked. *)

(** {2 Encoding/Decoding of events}
    The encoding and decoding of events currently uses Bencode. *)

val decode : Bencode_types.t -> t
(** Given a Bencode value, returns the event associated with it. May fail if
    the given value do not match an encoded event. *)

val encode : out_channel -> t -> unit
(** Output the encoded event to the given channel. *)

(** {2 Ids} *)

module Id : sig

  type t = id

  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int

  val to_string : t -> string

  val pp : Buffer.t -> t -> unit

end
