#load "type.cmo"
#load "id.cmo"
#load "m.cmo"
#load "s.cmo"
#load "parser.cmo"
#load "lexer.cmo"
#load "typing.cmo"
#load "syntax.cmo"
open Syntax

let string_contains (lhs : string) (rhs : string) : bool =
  String.to_seq rhs
  |> List.of_seq
  |> List.for_all (fun c -> String.contains lhs c)

let () =
  assert (string_contains "hoge" "hogeeee")

let parse s =
  Parser.exp Lexer.token (Lexing.from_string s);;

(* The following is a totally wrong unparser implementation. Please
 * ignore the definition and build an unparser from scratch. *)

let rec string_of_ast t =
  match t with
  | Unit -> "()"
  | Int (n) -> string_of_int n
  | Bool (b) -> string_of_bool b
  | Float (f) -> string_of_float f
  | Not (LE (t1, t2)) ->
    Printf.sprintf "(%s < %s)" (string_of_ast t2) (string_of_ast t1)
  | Not (t) -> Printf.sprintf "(not (%s))" (string_of_ast t)
  | Neg (t) -> Printf.sprintf "(- %s)" (string_of_ast t)
  | FNeg (t) -> Printf.sprintf "(-. %s)" (string_of_ast t)
  | Var (x) when (String.contains x 'T') -> "_"
  | Var (x) -> x
  | Add (t1, t2) ->
    Printf.sprintf "%s + %s"
      (string_of_ast t1) (string_of_ast t2)
  | Sub (t1, t2) ->
    Printf.sprintf "%s - %s"
      (string_of_ast t1) (string_of_ast t2)
  | FAdd (t1, t2) ->
    Printf.sprintf "%s +. %s"
      (string_of_ast t1) (string_of_ast t2)
  | FSub (t1, t2) ->
    Printf.sprintf "%s -. %s" (
      string_of_ast t1) (string_of_ast t2)
  | FMul (t1, t2) ->
    Printf.sprintf "%s *. %s"
      (string_of_ast t1) (string_of_ast t2)
  | FDiv (t1, t2) ->
    Printf.sprintf "%s /. %s"
      (string_of_ast t1) (string_of_ast t2)
  | Eq (t1, t2) ->
    Printf.sprintf "%s = %s"
      (string_of_ast t1) (string_of_ast t2)
  | LE (t1, t2) ->
    Printf.sprintf "%s <= %s"
      (string_of_ast t1) (string_of_ast t2)
  | If (t1, t2, t3) ->
    Printf.sprintf "if %s then %s else %s"
      (string_of_ast t1) (string_of_ast t2) (string_of_ast t3)
  | Let ((id, typ), t1, t2) ->
    begin
      if string_contains id "Tu" then
        Printf.sprintf "(%s; %s)"
          (string_of_ast t1) (string_of_ast t2)
      else
        Printf.sprintf "let %s = %s in %s"
          id (string_of_ast t1) (string_of_ast t2)
    end
  | LetRec (fundef, t) ->
    let { name; args; body } = fundef in
    let (id, _) = name in
    let args' = List.fold_left begin fun acc (id, _) ->
        if String.contains id 'T' && String.contains id 'u' then
          acc ^ ("_" ^ " ")
        else
          acc ^ (id ^ " ")
      end "" args in
    begin match t with
     | App (t, ts) ->
       Printf.sprintf "let rec %s %s= %s in %s (%s)"
         id args' (string_of_ast body) (string_of_ast t) (string_of_ast_ts ts)
     | _ ->
       Printf.sprintf "let rec %s %s= %s in %s" id
         args' (string_of_ast body) (string_of_ast t)
    end
  | App (t, ts) ->
    let rec loop = function
      | [] -> ""
      | hd :: [] -> "(" ^ (string_of_ast hd) ^ ")"
      | hd :: tl -> "(" ^ (string_of_ast hd) ^ ")" ^ " " ^ (loop tl)
    in
    Printf.sprintf "%s %s" (string_of_ast t) (loop ts)
  | Tuple (ts) ->
    Printf.sprintf "(%s)" (string_of_ast_ts ts)
  | LetTuple (xs, t1, t2) ->
    let rec loop = function
      | [] -> ""
      | (id, _) :: [] -> id
      | (id, _) :: tl -> id ^ ", " ^ (loop tl)
    in
    Printf.sprintf "let (%s) = %s in %s"
      (loop xs) (string_of_ast t1) (string_of_ast t2)
  | Array (t1, t2) ->
    Printf.sprintf "Array.make %s %s"
      (string_of_ast t1) (string_of_ast t2)
  | Get (t1, t2) ->
    Printf.sprintf "%s.(%s)" (string_of_ast t1) (string_of_ast t2)
  | Put (t1, t2, t3) ->
    Printf.sprintf "%s.(%s) <- %s"
      (string_of_ast t1) (string_of_ast t2) (string_of_ast t3)

and string_of_ast_ts = function
  | [] -> ""
  | hd :: [] -> string_of_ast hd
  | hd :: tl -> (string_of_ast hd) ^ ", " ^ (string_of_ast_ts tl)

let rec unparse (ast : Syntax.t) =
  string_of_ast ast

let small_test_programs = [
  "()"; "print(true)"; "print(5)"; "print(3.14)";
  "print(not (4 = 5))"; "print(1 + 2 - 3)";
  "print(-1.0 +. 2.0 -. 3.0 *. 4.0 /. 5.0)";
  "print(3 < 4)"; "print(5 < 6)";
  "print(let x = 1 in let y = x in x + y)";
  "print(let x = let y = 1 in let z = 2 in y + z in x)";
  "print(let x = let rec f x = x + 1 in f 1 in x)";
  "print(let x = (1, 2) in let (a, b) = x in a + b)";
  "print(let x = 1 in let y = 2 in if y - 1 = x then x + y else y)";
  "print(let rec f n = if n <= 1 then 1 else f(n - 2) + f(n - 1) in f 5)";
  "print(let rec f n = if n = 0 then 1 else n + f (n - 1) in f(10))";
  "print(let a = 1 in let rec incr x = x + a in incr 5)";
  "let rec f x = let x1 = x + 1 in let x2 = x + 2 in let x3 = x + 3 in let x4 = x + 4 in let x5 = x + 5 in x1 + x2 + x3 + x4 + x5 in print(f 1)";
  "print(let (a, b) = (1, 2) in a + b)";
  "print(let a = Array.make 5 1.0 in a.(0) <- a.(1))" ];;

let test_count = ref 1

let test label s =
  if label = "" then Printf.printf "Test case (%d): %s\n" (!test_count) s
  else Printf.printf "Test case (%d): [%s]\n" (!test_count) label;
  incr test_count;
  flush stdout;

  Id.counter := 0;
  let ast = parse s in
  ignore (Typing.f ast);  (* Assert that the program is well typed *)

  let s = try unparse ast with e ->
    print_endline "Failure during unparsing";
    raise e in

  Id.counter := 0;
  let pus = try parse s with e ->
    print_endline "Failure during parsing the output of your unparser";
    print_endline s;
    raise e in
  ignore (Typing.f pus);        (* added *)

  if pus = ast then print_endline "Beautiful!"
  else begin
    print_endline "Unsuccessful: Your unparser gave:";
    print_endline s;
  end;
  print_newline();;

(* Small tests *)
List.iter (test "") small_test_programs;;

(* Large tests *)
List.iter (fun path ->
  let fin = open_in path in
  let lines = ref [] in
  let program =
    try while true do lines := (input_line fin) :: (!lines) done; ""
    with End_of_file -> String.concat "\n" (List.rev (!lines)) in
  test path program)
  [ "shootout/ack.ml";
    "shootout/fib.ml";
    "shootout/harmonic.ml";
    "shootout/mandelbrot.ml";
    "shootout/tak.ml" ];;

(* Todo
 * - type check the tests
 * - exception handling
 *)
