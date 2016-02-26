
(* This file is free software, part of Neperien. See file "LICENSE" for more details. *)

module P = Neperien_Parse
module E = Neperien_Event

(*
   ### State
   Current state of the log reader
*)

type 'a plist = {
  start : int;
  vec : 'a CCVector.vector;
}

let _empty () = {
  start = 0;
  vec = CCVector.create ();
}

type state = {
  parse : P.t;
  context : E.t plist;
  children : E.t plist;
}

let mk filename = {
  parse = P.open_file_exn filename;
  context = _empty ();
  children = _empty ();
}

(*
   ### Rendering
*)

let img _ (_, _) =
  Notty.(I.string A.empty "Hello world!")

let render st term =
  Notty_lwt.Term.image term (img st (Notty_lwt.Term.size term))

(*
   ### Interface even handling
*)

let react _ = function
  | _ -> ()

let handle st term = function
  | `Key (`Escape, _) | `Key (`Uchar 113, _) (* 'q' *) ->
    Notty_lwt.Term.release term
  | event ->
    react st event;
    render st term

(*
   ### Main function
*)

let main filename =
  Lwt_main.run @@
  let term = Notty_lwt.Term.create () in
  let _ = Lwt_unix.on_signal Sys.sigint (fun _ ->
      Lwt_main.run @@ Notty_lwt.Term.release term) in
  let st = mk filename in
  let%lwt () = render st term in
  Lwt_stream.iter_s (handle st term) (Notty_lwt.Term.events term)

let argspec = Arg.align []

let usage =
  "./logn file"

let () =
  let file = ref "" in
  Arg.parse argspec (fun s -> file := s) usage;
  if !file = "" then begin
    Arg.usage argspec usage;
    exit 2
  end;
  main !file

