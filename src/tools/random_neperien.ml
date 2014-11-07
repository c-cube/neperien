
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

(** {1 Tool that generates random logs} *)

module N = Neperien
module RS = Random.State

let choice_ st a =
  assert (Array.length a > 0);
  a.(RS.int st (Array.length a)) st

let const_ x _st = x

let messages_ =
  [| const_ "I am a message!"
   ; const_ "hello"
   ; const_ "how are you dear?"
   ; const_ "CCList.Traverse(YoloMonad)"
   ; const_ "this"
   ; const_ "is"
   ; const_ "spartaaaaa"
   ; (fun st -> "so random: " ^ string_of_int (RS.int st 2048))
  |]

let rec random_log log len st =
  if len=0 then ()
  else if len=1 then rand_flat log len st
  else choice_ st [| rand_flat log len; rand_deep log len |]
and rand_flat log len st =
  let msg = choice_ st messages_ in
  N.send' log msg;
  random_log log (len-1) st
and rand_deep log len st =
  assert (len>=2);
  let msg = choice_ st messages_ in
  (* length of sub-sequence *)
  let sub_len = RS.int st (min (len-1) 100) in
  N.within' log msg (fun _ -> random_log log sub_len st);
  random_log log (len-1-sub_len) st

let process_file st file len =
  if file="" then failwith "file required";
  let log = N.log_to_file_exn file in
  random_log log len st

let file = ref ""
let add_file f = file := f
let len = ref 10_000
let seed = ref 0xdeadbeef

let args = Arg.align
  [ "-n", Arg.Set_int len, " number of messages"
  ; "-seed", Arg.Set_int seed, " random seed"
  ]

let () =
  Arg.parse args add_file "usage: random_neperien <file> [options]...";
  let st = RS.make [| !seed |] in
  process_file st !file !len
