open MinCaml
open Syntax

let parse l =
  Parser.exp Lexer.token l

let print_string s =
  Lexing.from_string s
  |> parse
  |> Syntax.show
  |> print_endline

let rec unparse t =
  match t with
  | Int (n) -> string_of_int n
  | Var (x) -> x
  | Add (t1, t2) -> Printf.sprintf "%s + %s" (unparse t1) (unparse t2)
  | Sub (t1, t2) -> Printf.sprintf "%s - %s" (unparse t1) (unparse t2)
  | FAdd (t1, t2) -> Printf.sprintf "%s +. %s" (unparse t1) (unparse t2)
  | FSub (t1, t2) -> Printf.sprintf "%s -. %s" (unparse t1) (unparse t2)
  | FMul (t1, t2) -> Printf.sprintf "%s *. %s" (unparse t1) (unparse t2)
  | FDiv (t1, t2) -> Printf.sprintf "%s /. %s" (unparse t1) (unparse t2)
  | Eq (t1, t2) -> Printf.sprintf "%s = %s" (unparse t1) (unparse t2)
  | LE (t1, t2) -> Printf.sprintf "%s <= %s" (unparse t1) (unparse t2)
  | If (t1, t2, t3) -> Printf.sprintf "if %s then %s\nelse %s" (unparse t1) (unparse t2) (unparse t3)
  | Let ((id, typ), t1, t2) -> Printf.sprintf "let %s = %s in %s" id (unparse t1) (unparse t2)
  | LetRec (fundef, t) ->
    let { name; args; body } = fundef in
    let (id, _) = name in
    let args' = List.fold_left (fun acc (id, _) -> id ^ " " ^ acc) "" args in
    Printf.sprintf "let rec %s %s= %s\nin %s" id args' (unparse body) (unparse t)
  | App (t, ts) ->
    let args = List.fold_left (fun acc t -> (unparse t) ^ " " ^ acc) "" ts in
    Printf.sprintf "%s %s" (unparse t) args
  | _ -> failwith "Unimplemented term."


let rec unparse_string s =
  Lexing.from_string s
  |> parse
  |> unparse
  |> print_endline
