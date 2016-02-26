
(* This file is free software, part of Neperien. See file "LICENSE" for more details. *)

(** {1 Simple Tool that prints content of a Log} *)

module P = Neperien_Parse
module E = Neperien_Event

let print_ev e =
  Printf.printf "event {id=%s; descr=\"%s\"; level=%d; "
    (E.Id.to_string e.E.id) e.E.descr e.E.level;
  begin match e.E.prev with
    | None -> ()
    | Some i -> Printf.printf "prev=%s; " (E.Id.to_string i)
  end;
  begin match e.E.last_child with
    | None -> ()
    | Some i -> Printf.printf "last_child=%s; " (E.Id.to_string i)
  end;
  Printf.printf "}\n";
  ()

let process_file file =
  match P.open_file file with
  | `Error msg ->
      Printf.eprintf "error when trying to open %s: %s" file msg;
      exit 1
  | `Ok log ->
      P.iter log print_ev

(** {2 Main} *)

let files = ref []
let add_file f = files := f :: !files

let () =
  Arg.parse [] add_file "usage: cat_neperien <file1>...";
  List.iter process_file !files;
  ()
