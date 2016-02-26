
(* This file is free software, part of Neperien. See file "LICENSE" for more details. *)

(** {1 Tool that generates random logs} *)

module N = Neperien.Emit
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
