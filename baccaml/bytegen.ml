open MinCaml
module B = BacCaml

type backend =
  | Virtual
  | Bytecode
  | PPBytecode
  | Interp
  | Nothing

type show_insts_map =
  [ `True
  | `False
  ]

type debug =
  [ `True
  | `False
  ]

let backend_type = ref Bytecode
let show_insts_map_type : show_insts_map ref = ref `False
let debug_flg : debug ref = ref `False

let with_debug f =
  match !debug_flg with
  | `True ->
    B.Config.vm_debug_flg := `True;
    f ()
  | `False -> f ()
;;

let with_show_insts f =
  match !show_insts_map_type with
  | `True -> B.Insts.Printer.pp_inst_map ()
  | `False -> f ()
;;

let rec lexbuf oc l =
  let open B in
  Id.counter := 0;
  Typing.extenv := M.empty;
  Parser.exp Lexer.token l
  |> Typing.f
  |> KNormal.f
  |> Alpha.f
  |> Util.(iter !limit)
  |> Closure.f
  |> Virtual.f
  |> Simm.f
  |> fun p ->
  match !backend_type with
  | Virtual -> p |> Emit.f |> Array.to_list |> Insts.Printer.pp_insts
  | PPBytecode -> p |> Emit.f |> Insts.Printer.pp_bytecode oc
  | Bytecode -> p |> Emit.f |> Insts.Printer.write_bytecode oc
  | Interp -> p |> Emit.f |> VM.run_asm |> ignore
  | Nothing -> ()
;;

let main f =
  let ic = open_in f in
  let oc = stdout in
  try
    let input = Lexing.from_channel ic in
    lexbuf oc input;
    close_in ic;
    close_out oc
  with
  | e ->
    close_in ic;
    close_out oc;
    raise e
;;

let () =
  let files = ref [] in
  B.(
    Arg.parse
      [ "-debug", Arg.Unit (fun _ -> debug_flg := `True), "run as debug mode"
      ; ( "-no-sh"
        , Arg.Unit (fun _ -> Config.(sh_flg := `False))
        , "disable stack hybridization" )
      ; ( "-virtual"
        , Arg.Unit (fun _ -> backend_type := Virtual)
        , "emit MinCaml IR" )
      ; ("-tail-opt"
        , Arg.Unit (fun _ -> Config.(tail_opt_flg := true))
        , "enable optimization for tail-recursive call")
      ; ( "-insts"
        , Arg.Unit (fun _ -> show_insts_map_type := `True)
        , "show instruction map" )
      ; ( "-pp"
        , Arg.Unit (fun _ -> backend_type := PPBytecode)
        , "emit bytecode for BacCaml" )
      ; ( "-interp"
        , Arg.Unit (fun _ -> backend_type := Interp)
        , "run as interpreter" )
      ])
    (fun s -> files := !files @ [ s ])
    "usage: %s [-virtual] [-insts] [-bytes] [-interp]";
  with_show_insts (fun _ -> with_debug (fun _ -> List.iter main !files))
;;
