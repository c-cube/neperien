
module P = NeperienParse

let cat parse =
  let open Notty in
  let a1 = A.(fg lightred) in
  let a2 = A.empty in
  I.(
    string a1 "Hello world !" <->
    string a2 (String.make 500 'a'))

let handle term = function
  | `Key (`Escape, _) -> Notty_lwt.Term.release term
  | _ -> Lwt.return_unit

let () =
  Lwt_main.run @@
  let term = Notty_lwt.Term.create () in
  let parse = P.open_file_exn Sys.argv.(1) in
  let%lwt () = Notty_lwt.Term.image term (cat parse) in
  Lwt_stream.iter_s (handle term) (Notty_lwt.Term.events term)

