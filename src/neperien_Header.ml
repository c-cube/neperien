
(* This file is free software, part of Neperien. See file "LICENSE" for more details. *)

module B = Bencode_types

(* Version numbers *)
type version = int
let current_version = 1

(* Headers *)
type t = {
  version : version;
}

let mk () = {
  version = 1;
}

let decode b =
  begin match b with
    | B.Integer 1 -> { version = 1; }
    | B.Integer v ->
      failwith ("unknown version: " ^ string_of_int v)
    | _ -> failwith "expected version header"
  end

let encode oc t =
  Printf.fprintf oc "i%de\n" t.version

