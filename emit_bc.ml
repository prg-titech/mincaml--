open Asm

exception Error of string

type inst =
  | ADD
  | SUB
  | MUL
  | NEG
  | LT
  | LTEq
  | Eq
  | JUMP_IF_ZERO
  | JUMP
  | RET
  | HALT
  | DUP
  | POP1
  | CONST
  | Literali of int
  | Literalf of float
  | Lref of string
  | Ldef of string

module Value = struct
  type t =
    | Int of int
    | Float of float

  let literal_of_value = function
    | Int i -> Literali i
    | Float f -> Literalf f

  let (=) = function
    | Int (i), Int (j) -> i = j
    | Int (i), Float (j) -> (float_of_int i) = j
    | Float (i), Int (j) -> i = (float_of_int j)
    | Float (i), Float (j) -> i = j

  let neg = function
    | Int i -> Int (-i)
    | Float f -> Float (-.f)

  let add x y = match x, y with
    | Int (i), Int (j) -> Int (i + j)
    | Int (i), Float (j) -> Float ((float_of_int i) +. j)
    | Float (i), Int (j) -> Float (i +. (float_of_int j))
    | Float (i), Float (j) -> Float (i +. j)

  let sub x y = match x, y with
    | Int (i), Int (j) -> Int (i - j)
    | Int (i), Float (j) -> Float ((float_of_int i) -. j)
    | Float (i), Int (j) -> Float (i -. (float_of_int j))
    | Float (i), Float (j) -> Float (i -. j)
end

type env = (string * Value.t) list

let empty_env : env = []

let extend_env (x : string) (v : Value.t) (env : env) : env =
  (x, v) :: env

let look_up (x : string) (env : env) : Value.t =
  try List.assoc x env with
  | Not_found ->
    raise (Error ("variable `" ^ x ^ "' is not found."))

let label_counter = ref 0

let gen_label _ =
  let l = !label_counter in
  label_counter := l + 1;
  "$" ^ (string_of_int l)

let reset _ = label_counter := 0

let rec compile_t env = function
  | Ans (e) -> []
  | Let ((x, typ), e, t) ->
      begin match e with
      | Nop -> []
      | Mov (y) ->
        let v = look_up y env in
        let env = extend_env x v env in
        [DUP; (Value.literal_of_value v)] @ (compile_t env t)
      | Set (n) ->
        let env = extend_env x (Int n) env in
        [CONST; Literali n] @ (compile_t env t)
      | Neg (y) ->
        let v = look_up y env in
        let env = extend_env x (Value.neg v) env in
        [NEG; (Value.literal_of_value v)] @ (compile_t env t)
      | Add (y, V (z)) ->
        let yv = look_up y env in
        let zv = look_up z env in
        let env = extend_env x (Value.add yv zv) env in
        [Value.literal_of_value yv;
         Value.literal_of_value zv;
         ADD;] @ (compile_t env t)
      | Add (y, C (n)) ->
        let yv = look_up y env in
        let env = extend_env x (Value.add yv (Int n)) env in
        [Value.literal_of_value yv;
         Literali n;
         ADD;] @ (compile_t env t)
      | Sub (y, V (z)) ->
        let yv = look_up y env in
        let zv = look_up z env in
        let env = extend_env x (Value.sub yv zv) env in
        [Value.literal_of_value yv;
         Value.literal_of_value zv;
         SUB;] @ (compile_t env t)
      | Sub (y, C (n)) ->
        let yv = look_up y env in
        let env = extend_env x (Value.sub yv (Int n)) env in
        [Value.literal_of_value yv;
         Literali n;
         SUB;] @ (compile_t env t)
      | IfEq (t1, V (t2), y, z) ->
        let l2, l1 = gen_label (), gen_label () in
        let t1 = look_up t1 env in
        let t2 = look_up t2 env in
        [Value.literal_of_value t1; Value.literal_of_value t2;
         Eq; JUMP_IF_ZERO; Lref l2]
        @ [Ldef l1;] @ (compile_t env y)
        @ [Ldef l2;] @ (compile_t env z)
      | _ -> raise (Error "unimplemented instruction.")
    end
