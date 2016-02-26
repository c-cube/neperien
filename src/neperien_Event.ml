
(* This file is free software, part of Neperien. See file "LICENSE" for more details. *)

module B = Bencode_types

type id = int

type t = {
  id : id;
  descr : string;
  causes : id list;
  level : int;            (* depth in tree *)
  prev : id option;       (* previous node in tree *)
  last_child: id option;  (* ID of last child, if any *)
}

(*
   Encoding/Decoding of events uses Bencode.
   Each event is a dict containing the following entries:
   - "c": list int  (causes IDs)
   - "d": string (description)
   - "i": int (id)
   - "l": int (level)
   - "lc": int option (last child ID)
   - "p": int option (previous ID)
  *)

(*
   ### Decoding
   Decoding uses the normal functions of bencode to read
   the dict and extract the fields
*)

let get_field name b =
  try List.assoc name b
  with Not_found -> failwith ("expected field " ^ name)

let get_int b = match b with
  | B.Integer i -> i
  | _ -> failwith "expected int"

let get_str b = match b with
  | B.String s -> s
  | _ -> failwith "expected string"

let get_l get b = match b with
  | B.List l' -> List.map get l'
  | _ -> failwith "expected list"

let decode b = match b with
  | B.Dict l ->
      let id = get_int (get_field "i" l) in
      let descr = get_str (get_field "d" l) in
      let causes =
        try get_l get_int (List.assoc "c" l)
        with Not_found -> []
      in
      let level = get_int (get_field "l" l) in
      let prev = try Some (get_int (List.assoc "p" l)) with Not_found -> None in
      let last_child = try Some(get_int (List.assoc "lc" l)) with Not_found -> None in
      { id; descr; causes; level; prev; last_child; }
  | _ -> failwith "expected dict"

(*
   ### Encoding
   Encoding of messages do NOT use bencode functions in an attempts to
   avoid the allocation of a bencode AST and thus achieve maximum speed.
   TODO: Add some unit tests to check that the custom printing of bencode values
         match the actual bencode format.
*)

let pp_id_list oc l =
  output_char oc 'l';
  List.iter (fun id -> Printf.fprintf oc "i%de" id) l;
  output_char oc 'e';
  ()

let encode oc e =
  output_char oc 'd';
  (* causes *)
  if e.causes <> [] then (
    output_string oc "1:c";
    pp_id_list oc e.causes;
  );
  (* descr *)
  Printf.fprintf oc "1:d%d:%s" (String.length e.descr) e.descr;
  (* ID *)
  Printf.fprintf oc "1:ii%de" e.id;
  (* level *)
  Printf.fprintf oc "1:li%de" e.level;
  (* last child *)
  begin match e.last_child with
    | None -> ()
    | Some i -> Printf.fprintf oc "2:lci%de" i
  end;
  (* previous *)
  begin match e.prev with
    | None -> ()
    | Some i -> Printf.fprintf oc "1:pi%de" i
  end;
  (* end *)
  output_string oc "e\n";
  ()

(*
   ### Ids
   The following module offers convenient functions for manipulating ids
*)

module Id = struct
  type t = id

  let compare = Pervasives.compare
  let hash (i : id) = Hashtbl.hash i
  let equal (i : id) j = Pervasives.(=) i j

  let to_string = string_of_int

  let pp buf = Printf.bprintf buf "%d"
end

