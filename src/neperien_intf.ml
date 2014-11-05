
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


(** {1 Module Interface} *)

type id = int
type level = int

module type S = sig
  val send : ?causes:id list -> string -> id
  (** New id, with an informal description (the string parameter). It depends
      on some previous ids (the [causes] list), and some more global context
      (ongoing event/task, see [within]). *)

  val send_b : ?causes:id list ->
               ('a, Buffer.t, unit, id) format4 -> 'a

  val within : ?causes:id list -> string -> (unit -> 'a) -> 'a * id

  val within_b : ?causes:id list ->
                ('a, Buffer.t, unit, ((unit -> 'b) -> 'b * id)) format4 -> 'a

  val close : unit -> unit

  module Unsafe : sig
    val within_enter : unit -> level
    (** Enter a "within" context, same as {!within}. {b Note}: careful,
        if you forget to call {!within_exit} (especially in case of exception)
        it could mess up the hierarchy. *)

    val within_exit : level -> ?causes:id list -> string -> id
    (** Exit the "within" context, returning the ID of the corresponding event.
        @raise Failure if the level doesn't match *)

    val within_exit_b : level -> ?causes:id list ->
                          ('a, Buffer.t, unit, id) format4 -> 'a
  end
end
