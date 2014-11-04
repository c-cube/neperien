#!/usr/bin/env ocaml
#use "topfind";;
#directory "_build/src";;
#load "structLog.cma";;
let file = "/tmp/yolog";;
let log = match StructLog.log_to_file file with
  | `Error msg -> failwith msg
  | `Ok x -> x;;
let a = StructLog.make log "a";;
let b = StructLog.make log ~within:a "b";;
let c = StructLog.make log ~causes:[a;b] "c";;
StructLog.close log;;
print_endline ("consult log file " ^ file);;
