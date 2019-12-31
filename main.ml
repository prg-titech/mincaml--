open MinCaml

type backend = MinCaml | Wasm | VirtualDump | BC

type debug = Ast | False

let backend_type = ref MinCaml

let debug = ref False

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
    | Wasm -> Emit_wasm.f oc p
    | MinCaml -> RegAlloc.f p |> Emit.f oc
    | VirtualDump -> Asm.show_prog p |> Printf.fprintf oc "%s"
    | BC ->
      BacCaml.Emit.(
        f p
        |> Array.map (fun inst -> show_inst inst |> Printf.fprintf oc "%s\n")
        |> ignore)
  end

let string s = lexbuf stdout (Lexing.from_string s)

let file f =
  let inchan = open_in (f ^ ".ml") in
  let outchan =
    match !backend_type with
    | Wasm -> open_out (f ^ ".wat")
    | MinCaml -> open_out (f ^ ".s")
    | VirtualDump -> stdout
    | BC -> stdout
  in
  try
    let input = Lexing.from_channel inchan in
    match !debug with
    | Ast -> ast stdout input
    | False -> lexbuf outchan input ;
      close_in inchan ;
      close_out outchan
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
      , Arg.Unit (fun _ -> debug := Ast)
      , "emit abstract syntax tree")
    ; ( "-bc"
      , Arg.Unit (fun _ -> backend_type := BC)
      , "")
    ; ("-wasm", Arg.Unit (fun _ -> backend_type := Wasm), "emit webassembly")
    ]
    (fun s -> files := !files @ [s])
    ( "Mitou Min-Caml Compiler (C) Eijiro Sumii\n"
    ^ Printf.sprintf
        "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..."
        Sys.argv.(0) ) ;
  List.iter
    (fun f -> file (Filename.remove_extension f))
    !files
