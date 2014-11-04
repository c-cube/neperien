#!/usr/bin/env ocaml
#use "topfind";;
#directory "_build/src";;
#load "structLog.cma";;
let file = "/tmp/yolog";;
let log = match StructLog.log_to_file file with
  | `Error msg -> failwith msg
  | `Ok x -> x;;
let a = StructLog.make log "a";;
StructLog.within log a
  (fun () ->
    let b = StructLog.make log "b" in
    let _c = StructLog.make log ~causes:[b] "c" in
    ()
  );;
StructLog.close log;;
print_endline ("consult log file " ^ file);;
