open MinCaml

type backend =
  | MinCaml
  | Wasm
  | Virtual

let backend_type = ref MinCaml

let debug = ref false

let baccaml_instr_info = ref false

let ast_dump = ref false

let with_flag flag ~tru:f ~fls:g =
  if !flag then f () else g ()

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
  |> Util.(iter !limit)
  |> Closure.f
  |> Virtual.f
  |> Simm.f
  |> fun p ->
  begin
    match !backend_type with
    | Wasm -> Wasm.Emit.f oc p
    | MinCaml -> RegAlloc.f p |> X86.Emit.f oc
    | Virtual -> Asm.show_prog p |> Printf.fprintf oc "%s"
  end

let string s = lexbuf stdout (Lexing.from_string s)

let main f =
  let inchan = open_in f in
  let outchan =
    let f = Filename.remove_extension f in
    match !backend_type with
    | Wasm -> open_out (f ^ ".wat")
    | MinCaml -> open_out (f ^ ".s")
    | _ -> stdout
  in
  try
    let input = Lexing.from_channel inchan in
    with_flag ast_dump
      ~tru:(fun _ -> ast outchan input)
      ~fls:(fun _ -> lexbuf outchan input ;
             close_in inchan ;
             close_out outchan)
  with e -> close_in inchan ; close_out outchan ; raise e

let () =
  let files = ref [] in
  Arg.parse
    [ ( "-inline"
      , Arg.Int (fun i -> Inline.threshold := i)
      , "maximum size of functions inlined" )
    ; ( "-iter"
      , Arg.Int (fun i -> Util.limit := i)
      , "maximum number of optimizations iterated" )
    ; ( "-ast"
      , Arg.Unit (fun _ -> ast_dump := true)
      , "emit abstract syntax tree")
    ; ("-virtual"
      , Arg.Unit (fun _ -> backend_type := Virtual)
      , "emit virtual machine code")
    ; ("-debug"
      , Arg.Unit (fun _ -> debug := true)
      , "enable debug mode")
    ; ( "-bc-inst"
      , Arg.Unit (fun _ -> baccaml_instr_info := true)
      , "show instruction map")
    ; ("-wasm", Arg.Unit (fun _ -> backend_type := Wasm), "emit webassembly")
    ]
    (fun s -> files := !files @ [s])
    ( "Mitou Min-Caml Compiler (C) Eijiro Sumii\n"
    ^ Printf.sprintf
        "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..."
        Sys.argv.(0) ) ;
  List.iter
    (fun f -> main f)
    !files
