
(* This file is free software, part of Neperien. See file "LICENSE" for more details. *)

(** {2 Version numbers *)

type version = int
(** Version number. *)

val current_version : version
(** The current version number *)

(** {2 Headers} *)

type t = private {
  version: version;
}
(** The type for headers, currently only contains the version number. *)

val mk : unit -> t
(** Create a header *)

val decode : Bencode_types.t -> t
(** Tries and decode the given Bencode value as a header. Might fail. *)

val encode : out_channel -> t -> unit
(** Encode and outputs the given header to the given channel *)

