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

(** {1 Simple Tool that prints content of a Log} *)

module P = NeperienParse

let print_ev e =
  Printf.printf "event {id=%s; descr=\"%s\"; level=%d"
    (P.string_of_id e.P.id) e.P.descr e.P.level;
  begin match e.P.prev with
    | None -> ()
    | Some i -> Printf.printf "prev=%s; " (P.string_of_id i)
  end;
  begin match e.P.last_child with
    | None -> ()
    | Some i -> Printf.printf "last_child=%s; " (P.string_of_id i)
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
