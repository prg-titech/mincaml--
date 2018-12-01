open MinCaml
let limit = ref 1000
let is_unparse = ref false
let is_wasm = ref false

let rec iter n e =
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
  let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
  if e = e' then e else
  iter (n - 1) e'

let lexbuf oc l =
  Id.counter := 0;
  Typing.extenv := M.empty;
  Parser.exp Lexer.token l
  |> Typing.f
  |> KNormal.f
  |> Alpha.f
  |> iter !limit
  |> Closure.f
  |> Virtual.f
  |> Simm.f
  |> fun p ->
    (if !is_wasm then
      Emit_wasm.f oc p
     else
       RegAlloc.f p
       |> Emit.f oc)

let string s = lexbuf stdout (Lexing.from_string s)

let file f =
  let inchan = open_in (f ^ ".ml") in
  let outchan =
    if !is_wasm then
      open_out (f ^ ".wat")
    else
      open_out (f ^ ".s")
  in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () =
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i),
      "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i),
      "maximum number of optimizations iterated");
     ("-wasm", Arg.Unit (fun _ -> is_wasm := true),
     "emit webassembly")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files
