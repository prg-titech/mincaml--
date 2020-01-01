#!/usr/bin/env ocaml
open Printf
module F = Filename

let s = sprintf
let cmd = Sys.command

let () =
  let target = Sys.argv.(1) in
  if F.check_suffix target ".bc" then
    let f = F.chop_extension target in
    if cmd (s "dune exec min-caml -- %s" target) = 0 then
      ignore (cmd (
        s "gcc -m32 -O2 -Wall src/stub.c src/libmincaml.S %s -lm -o %s" (f ^ ".s") (f ^ ".exe")))
    else
      failwith "compilation failed."
  else ()
