#!/usr/bin/env ocaml

open Printf
module F = Filename

let s = sprintf
let cmd = Sys.command
let with_error_code f = if f () = 0 then () else raise Exit
let emit_x86 f = cmd @@ s "dune exec min-caml -- %s" f
let emit_bytecode f = cmd @@ s "dune exec bytegen -- %s" f

let compile_with_gcc f =
  cmd
  @@ s
       "gcc -m32 -O2 -Wall src/stub.c src/libmincaml.S %s -lm -o %s"
       (f ^ ".s")
       (f ^ ".exe")
;;

let () =
  let file = Sys.argv.(1) in
  let name, ext = F.chop_extension file, F.extension file in
  match ext with
  | ".mcml" ->
    with_error_code (fun () -> emit_x86 file);
    with_error_code (fun () -> compile_with_gcc name)
  | ".ml" -> with_error_code (fun _ -> emit_bytecode file)
  | _ -> ()
;;
