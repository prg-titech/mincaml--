open MinCaml

type backend =
  | MinCaml
  | Wasm
  | VirtDump
  | BacCamlVirtDump
  | BacCamlInterp

type debug = True | False

type baccaml_instr_info = True | False

type ast_dump = True | False

let backend_type = ref MinCaml

let debug = ref False

let baccaml_instr_info = ref False

let ast_dump = ref False

let limit = ref 1000

let rec iter n e =
  Format.eprintf "iteration %d@." n ;
  if n = 0 then e
  else
    let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
    if e = e' then e else iter (n - 1) e'

let ast oc l =
  Id.counter := 0;
  Parser.exp Lexer.token l
  |> Syntax.show
  |> print_endline

let lexbuf oc l =
  Id.counter := 0 ;
  Typing.extenv := M.empty ;
  Parser.exp Lexer.token l
  |> Typing.f
  |> KNormal.f
  |> Alpha.f
  |> iter !limit
  |> Closure.f
  |> Virtual.f
  |> Simm.f
  |> fun p ->
  begin
    match !backend_type with
    | Wasm -> Wasm.Emit.f oc p
    | MinCaml -> RegAlloc.f p |> X86.Emit.f oc
    | VirtDump -> Asm.show_prog p |> Printf.fprintf oc "%s"
    | BacCamlVirtDump -> BacCaml.(Emit.(f p |> Array.to_list |> Insts.pp_insts |> ignore))
    | BacCamlInterp -> ignore (BacCaml.(VM.run_asm (Emit.f p)))
  end

let string s = lexbuf stdout (Lexing.from_string s)

let main f =
  let inchan = open_in (f ^ ".ml") in
  let outchan =
    match !backend_type with
    | Wasm -> open_out (f ^ ".wat")
    | MinCaml -> open_out (f ^ ".s")
    | _ -> stdout
  in
  try
    let input = Lexing.from_channel inchan in
    match !ast_dump with
    | True -> ast outchan input
    | False -> begin
        lexbuf outchan input ;
        close_in inchan ;
        close_out outchan
      end
  with e -> close_in inchan ; close_out outchan ; raise e

let () =
  let files = ref [] in
  Arg.parse
    [ ( "-inline"
      , Arg.Int (fun i -> Inline.threshold := i)
      , "maximum size of functions inlined" )
    ; ( "-iter"
      , Arg.Int (fun i -> limit := i)
      , "maximum number of optimizations iterated" )
    ; ( "-ast"
      , Arg.Unit (fun _ -> ast_dump := True)
      , "emit abstract syntax tree")
    ; ("-virtual"
      , Arg.Unit (fun _ -> backend_type := VirtDump)
      , "emit virtual machine code")
    ; ("-debug"
      , Arg.Unit (fun _ -> debug := True)
      , "enable debug mode")
    ; ( "-bc-dump"
      , Arg.Unit (fun _ -> backend_type := BacCamlVirtDump)
      , "emit bytecode instrunctions for baccaml")
    ; ( "-bc-interp"
      , Arg.Unit (fun _ -> backend_type := BacCamlInterp)
      , "run bytecode instrunctions")
    ; ( "-bc-inst"
      , Arg.Unit (fun _ -> baccaml_instr_info := True)
      , "show instruction map")
    ; ("-wasm", Arg.Unit (fun _ -> backend_type := Wasm), "emit webassembly")
    ]
    (fun s -> files := !files @ [s])
    ( "Mitou Min-Caml Compiler (C) Eijiro Sumii\n"
    ^ Printf.sprintf
        "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..."
        Sys.argv.(0) ) ;
  (match !baccaml_instr_info with
   | True ->
     BacCaml.(Insts.pp_inst_map ())
   | False ->
     (match !debug with True -> BacCaml.VM.debug_flg := true | False -> ());
     List.iter
       (fun f -> main (Filename.remove_extension f))
       !files);