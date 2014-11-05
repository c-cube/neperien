#!/usr/bin/env ocaml
#use "topfind";;
#directory "_build/src";;
#load "neperien.cma";;
let file = "/tmp/yolog";;
module L = Neperien;;
let log = match L.log_to_file file with
  | `Error msg -> failwith msg
  | `Ok x -> x;;
L.within log "greetings, all that"
  (fun _id ->
    let b = L.make log "hello" in
    let _c = L.make log ~causes:[b] "oh, cheers!" in
    ()
  );;
L.close log;;
print_endline ("consult log file " ^ file);;
