open Printf

let s = sprintf
let cmd = Sys.command

let () =
  let interp = Sys.argv.(1) in
  let f = Filename.chop_extension interp in
  if cmd (s "dune exec min-caml -- %s" interp) = 0 then
    ignore (cmd(s "gcc -m32 -O2 -Wall src/stub.c src/libmincaml.S %s -lm -o %s" (f ^ ".s") f))
  else
    failwith "compilation failed."
