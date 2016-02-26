
(* This file is free software, part of Neperien. See file "LICENSE" for more details. *)

type 'a sequence = ('a -> unit) -> unit
type 'a or_error = [`Ok of 'a | `Error of string ]

module E = Neperien_Event
module H = Neperien_Header

(** Misc *)


(*
   ### Logger
   Loggers maintain ce stack of current scopes
   TODO: use forkable handlers
*)

type cell = {
  mutable cur_child : E.id option;
  sc_prev : E.id option;
}

type t = {
  mutable closed : bool;
  filename : string;
  out : out_channel;
  mutable max_level : int;
  mutable stack_len : int;
  mutable stack : cell list;
}


(*
   ### Actual logging
   Someone else should explain how it works, :p
*)

let get_id t = pos_out t.out

(*
   TODO: this function is not called anywhere, bug ?
let set_max_level t l =
  t.max_level <- l
*)

let emit_event t e =
  E.encode t.out e

let _prev t = match t.stack with
  | [] -> None
  | sc :: _ -> sc.cur_child

let _make t causes descr last_child =
  let id = get_id t in
  let level = t.stack_len + 1 in
  (* maintain the prev/child pointers *)
  let prev = _prev t in
  begin match t.stack with
    | [] -> ()
    | sc :: _ -> sc.cur_child <- Some id
  end;
  E.{ id; descr; causes; prev; level; last_child }

let send t ?(causes=[]) descr =
  let e = _make t causes descr None in
  if e.E.level <= t.max_level then emit_event t e;
  e.E.id

let send_b t ?causes fmt =
  let buf = Buffer.create 24 in
  Printf.kbprintf
    (fun buf -> send t ?causes (Buffer.contents buf))
    buf fmt

let _push t =
  let level = t.stack_len + 1 in
  t.stack_len <- level;
  t.stack <- { sc_prev = _prev t; cur_child = None; } :: t.stack;
  level

let _pop t level =
  if level != t.stack_len
  then failwith "pop: mismatched levels";
  match t.stack with
  | [] -> assert false
  | sc :: tail ->
    t.stack <- tail;
    t.stack_len <- t.stack_len - 1;
    sc

let within t ?(causes=[]) msg f =
  let level = _push t in
  try
    let x = f () in
    let sc = _pop t level in
    (* create event, pointing to its last child, if any *)
    let last_child = sc.cur_child in
    let e = _make t causes msg last_child in
    (*  emit now, the event is complete *)
    if e.E.level <= t.max_level then emit_event t e;
    x, e.E.id
  with ex ->
    ignore (_pop t level);
    raise ex

let within_b t ?causes fmt =
  let buf = Buffer.create 24 in
  Printf.kbprintf (fun buf -> fun f ->
      within t ?causes (Buffer.contents buf) f)
    buf fmt

let send' t ?causes msg =
  ignore(send t ?causes msg)

let send_b' t ?causes fmt = 
  let buf = Buffer.create 24 in
  Printf.kbprintf (fun buf ->
      ignore(send t ?causes (Buffer.contents buf)))
    buf fmt

let within' t ?causes msg f =
  fst (within t ?causes msg f)

let within_b' t ?causes fmt =
  let buf = Buffer.create 24 in
  Printf.kbprintf (fun buf -> fun f ->
      fst (within t ?causes (Buffer.contents buf) f))
    buf fmt


(*
   ### Unsafe interface
   For those who do not fear manipulating everything explicitly, :)
*)

type level = int

module Unsafe = struct

  let within_enter t = _push t

  let within_exit t lvl ?(causes=[]) msg =
    let sc = _pop t lvl in
    (* create event, pointing to its last child, if any *)
    let last_child = sc.cur_child in
    let e = _make t causes msg last_child in
    (*  emit now, the event is complete *)
    if e.E.level <= t.max_level then emit_event t e;
    e.E.id

  let within_exit_b t level ?causes fmt =
    let buf = Buffer.create 24 in
    Printf.kbprintf (fun buf ->
        within_exit t level ?causes (Buffer.contents buf))
      buf fmt

  let within_exit' t level ?causes msg =
    ignore (within_exit t level ?causes msg)

  let within_exit_b' t level ?causes fmt =
    let buf = Buffer.create 24 in
    Printf.kbprintf (fun buf ->
        ignore (within_exit t level ?causes (Buffer.contents buf)))
      buf fmt
end

(*
   ### Logger creation
   Delayed until the end of the file because we need to do some operations
   inorder to ensure that the root of the events can be accessed
*)

let close t =
  if not t.closed then begin
    ignore (Unsafe.within_exit t 1 "")
  end;
  flush t.out;
  close_out_noerr t.out;
  t.closed <- true

let mk filename =
  let out = open_out_gen [Open_append; Open_creat; Open_trunc] 0o644 filename in
  (* start values *)
  let closed = false in
  let stack = [] in
  let stack_len = 0 in
  let max_level = max_int in
  let t = { closed; filename; out; stack_len; stack; max_level; } in
  (* initialization *)
  H.encode out (H.mk ());
  let l = Unsafe.within_enter t in
  assert (l = 1);
  Gc.finalise close t;
  (* Return value *)
  t

let log_to_file_exn = mk

let log_to_file f =
  try
    `Ok (log_to_file_exn f)
  with e ->
    `Error (Printexc.to_string e)
