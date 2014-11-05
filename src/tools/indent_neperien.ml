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

(** {1 Print logs with indentation} *)

module P = NeperienParse
module Box = Containers_misc.PrintBox

(* [e]: event, output: a box *)
let rec to_box log e =
  let hd = CCPrint.sprintf "{id=%s; causes=%a}"
    (P.string_of_id e.P.id) (CCList.pp P.pp_id) e.P.causes
  in
  let self_box = Box.(vlist ~bars:false [text hd; text e.P.descr]) in
  match e.P.last_child with
  | None -> self_box
  | Some _ ->
      let children = P.iter_children log e
        |> CCSequence.map (to_box log)
        |> CCSequence.to_rev_list in
      Box.tree ~indent:2 self_box children

let print_ev log e =
  let b = to_box log e in
  Box.output ~indent:2 stdout b

let process_file file =
  match P.open_file file with
  | `Error msg ->
      Printf.eprintf "error when trying to open %s: %s" file msg;
      exit 1
  | `Ok log ->
      P.iter_below log ~level:1 (print_ev log)

(** {2 Main} *)

let files = ref []
let add_file f = files := f :: !files

let () =
  Arg.parse [] add_file "usage: cat_neperien <file1>...";
  List.iter process_file !files;
  ()


