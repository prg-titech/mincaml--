open MinCaml
open BacCaml

type backend = Virtual | Bytecode | Interp | Nothing

type show_insts_map = True | False

type debug = True | False

let backend_type = ref Bytecode

let show_insts_map_type = ref False

let debug = ref False

let with_debug f =
  match !debug with
  | True -> VM.debug_flg := true; f ()
  | False -> f ()

let rec lexbuf oc l =
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
  |> fun p -> begin
    match !backend_type with
    | Virtual -> p |> Asm.show_prog |> print_endline
    | Bytecode -> p |> Emit.f |> Insts.Printer.pp_bytecode oc
    | Interp -> p |> Emit.f |> VM.run_asm |> ignore
    | Nothing -> ()
  end

let main f =
  let ic = open_in f in
  let oc = stdout in
  try
    let input = Lexing.from_channel ic in
    lexbuf oc input; close_in ic; close_out oc
  with e ->
    close_in ic; close_out oc; raise e

let () =
  let files = ref [] in
  Arg.parse
    [ ("-debug", Arg.Unit (fun _ -> debug := True), "run as debug mode");
      ("-virtual", Arg.Unit (fun _ -> backend_type := Virtual), "emit MinCaml IR");
      ("-insts", Arg.Unit (fun _ -> show_insts_map_type := True), "show instruction map");
      ("-bytes", Arg.Unit (fun _ -> backend_type := Bytecode), "emit bytecode for BacCaml");
      ("-interp", Arg.Unit (fun _ -> backend_type := Interp), "run as interpreter") ]
    (fun s -> files := !files @ [s])
    ("usage: %s [-virtual] [-insts] [-bytes] [-interp]");
  begin match !show_insts_map_type with
    | True -> Insts.Printer.pp_inst_map ()
    | False -> with_debug (fun _ -> List.iter main !files)
  end
