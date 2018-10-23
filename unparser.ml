open Syntax

let ast_of_string s =
  Lexing.from_string s
  |> Parser.exp Lexer.token
  |> Syntax.show

let print_ast s =
  ast_of_string s |> print_endline

let rec string_of_ast t =
  match t with
  | Unit -> "_"
  | Int (n) -> string_of_int n
  | Bool (b) -> string_of_bool b
  | Float (f) -> string_of_float f
  | Not (t) -> Printf.sprintf "!%s" (string_of_ast t)
  | Neg (t) -> Printf.sprintf "(- %s)" (string_of_ast t)
  | FNeg (t) -> Printf.sprintf "(-. %s)" (string_of_ast t)
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
    Printf.sprintf "if %s then %s\nelse %s"
      (string_of_ast t1) (string_of_ast t2) (string_of_ast t3)
  | Let ((id, typ), t1, t2) ->
    Printf.sprintf "let %s = %s in %s"
      id (string_of_ast t1) (string_of_ast t2)
  | LetRec (fundef, t) ->
    let { name; args; body } = fundef in
    let (id, _) = name in
    let args' = List.fold_left begin fun acc (id, _) ->
        if String.contains id 'T' && String.contains id 'u' then
          acc ^ ("_" ^ " ")
        else
          acc ^ (id ^ " ")
      end "" args in
    Printf.sprintf "let rec %s %s= %s\nin %s" id
      args' (string_of_ast body) (string_of_ast t)
  | App (t, ts) ->
    let rec loop = function
      | [] -> ""
      | hd :: [] -> string_of_ast hd
      | hd :: tl -> (string_of_ast hd) ^ " " ^ (loop tl)
    in
    Printf.sprintf "%s (%s)" (string_of_ast t) (loop ts)
  | Tuple (ts) ->
    Printf.sprintf "(%s)" (string_of_ast_ts ts)
  | LetTuple (xs, t1, t2) ->
    let rec loop = function
      | [] -> ""
      | (id, _) :: [] -> id
      | (id, _) :: tl -> id ^ " " ^ (loop tl)
    in
    Printf.sprintf "let %s = %s in %s"
      (loop xs) (string_of_ast t1) (string_of_ast t2)
  | Array (t1, t2) ->
    Printf.sprintf "Array.create %s %s"
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

let unparse_string s =
  Lexing.from_string s
  |> Parser.exp Lexer.token
  |> string_of_ast
  |> print_endline

let unparse ast = string_of_ast ast |> print_endline
