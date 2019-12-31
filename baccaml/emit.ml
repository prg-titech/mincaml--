open MinCaml
open Insts

exception Error of string

(* generate a unique label id *)
let gen_label, reset =
  let counter = ref 0 in
  ((fun () -> let l = !counter in
     counter := (!counter + 1);
     ("$"^(string_of_int l))),
   fun () -> counter := 0)

(* compilation environment maps local variable names to local
   variable numbers *)
let lookup env var =
  match List.find_opt (fun (_,v) -> var = v)
          (List.mapi (fun idx v -> (idx,v)) env) with
  | Some v -> fst v
  | None -> failwith (Printf.sprintf "%s not found" var)
let extend_env env var = var :: env
let shift_env env = extend_env env "*dummy*"
let return_address_marker = "$ret_addr"
let build_arg_env args = return_address_marker::(List.rev args)
(* computes the number of arguments to this frame.  The stack has a
   shape like [...local vars...][ret addr][..args...], the return
   address position from the top indicates the number of local
   variables on top of the return address. *)
let arity_of_env env =
  let num_local_vars = lookup env return_address_marker in
  (List.length env - num_local_vars  - 1, num_local_vars)

let label_counter = ref 0

let gen_label _ =
  let l = !label_counter in
  label_counter := l + 1;
  "$" ^ (string_of_int l)

let reset _ = label_counter := 0

let compile_id_or_imm env = function
  | Asm.C (n) -> [CONST; Literal n]
  | Asm.V (x) -> [DUP; Literal (lookup env x)]

let rec compile_t env = function
  | Asm.Ans (e) -> compile_exp env e
  | Asm.Let ((x,_), exp, t) ->
    let ex_env = extend_env env x in
    (compile_exp env exp) @
    (compile_t ex_env t) @
    [POP1]

and compile_exp env =
  let open Asm in
  function
  | Nop -> []
  | Set i -> [CONST; Literal i]
  | Mov var -> [DUP; Literal (lookup env var)]
  | Add (x, y) ->
    [DUP; Literal (lookup env x)] @
    (compile_id_or_imm (shift_env env) y) @
    [ADD]
  | Sub (x, y) ->
    [DUP; Literal (lookup env x)] @
    (compile_id_or_imm (shift_env env) y) @
    [SUB]
  | IfLE (x, y, then_exp, else_exp) ->
    let l2,l1 = gen_label(),gen_label () in
    [DUP; Literal (lookup env x)] @
    (compile_id_or_imm env y) @
    [LT] @
    [JUMP_IF_ZERO; Lref l1] @
    (compile_t env then_exp) @
    [CONST; Literal 0; JUMP_IF_ZERO; Lref l2] @
    [Ldef l1] @
    (compile_t env else_exp) @
    [Ldef l2]
  | IfGE (x, y, e1, e2)
  | IfEq (x, y, e1, e2) -> compile_exp env (IfLE (x, y, e2, e1))
  | CallDir (Id.L "min_caml_print_int", [x], _) ->
    (compile_id_or_imm env (V x)) @
    [PRINT_INT]
  | CallDir (Id.L "min_caml_print_newline", _, _) ->
    [PRINT_NEWLINE]
  | CallDir (Id.L "min_caml_create_array", [x; y], _) ->
    (compile_id_or_imm env (V x)) @
    (compile_id_or_imm (shift_env env) (V y)) @
    [ARRAY_MAKE]
  | CallDir (Id.L var, rands, _) ->
    ((List.fold_left (fun (rev_code_list, env) v ->
         [DUP; Literal (lookup env v)] :: rev_code_list,
         shift_env env)
        ([], env) rands)
     |> fst
     |> List.rev
     |> List.flatten) @
     [CALL; Lref var; Literal (List.length rands)]
  | Ld (x, y, _) ->
    [DUP; Literal (lookup env x)] @
    (compile_id_or_imm (shift_env env) y) @
    [GET]
  | St (x, y, z, _) ->
    [DUP; Literal (lookup env x)] @
    [DUP; Literal (lookup (shift_env env) y)] @
    (compile_id_or_imm (shift_env (shift_env env)) z) @
    [PUT]
  | exp ->
    failwith (Printf.sprintf "un matched pattern: %s" (Asm.show_exp exp))

module List= ListLabels

(* resolving labels *)
let assoc_if subst elm =
  try List.assoc elm subst with
  | Not_found  -> elm

(* [...;Ldef a;...] -> [...;a,i;...] where i is the index of the
   next instruction of Ldef a in the list all Ldefs are removed
   e.g., [_;Ldef 8;_;Ldef 7;_] ==> [8,1; 7,2]
*)
let make_label_env instrs =
  snd(List.fold_left
        ~f:(fun (addr,env) -> function
           | Ldef n -> (addr, (Lref n, Literal(addr))::env)
           | _ ->      (addr+1, env))
        ~init:(0,[]) instrs)

(* remove all Ldefs and replace Lrefs with Literals *)
let resolve_labels instrs =
  let lenv = make_label_env instrs in
  instrs
  |> List.map ~f:(assoc_if lenv)
  |> List.filter ~f:(function Ldef _ -> false | _ -> true)


let compile_fun_body fenv name arity exp env =
  [Ldef name] @
  (compile_t env exp) @
  (if name = "main" then [HALT] else [RET; Literal arity])


let compile_fun (fenv : Id.l -> Asm.fundef) Asm.{name= Id.L name; args; body;} =
  compile_fun_body fenv name (List.length args)
    body (build_arg_env args)


let compile_funs fundefs =
  (* let fenv name = fst(List.find (fun (_,{name=n}) -> name=n)
   *                       (List.mapi (fun idx fdef -> (idx,fdef))
   *                          fundefs)) in *)
  let fenv name = List.find ~f:(fun Asm.{name=n} -> n=name) fundefs in
  Array.of_list(resolve_labels
                  (List.flatten
                     (List.map ~f:(compile_fun fenv) fundefs)))

let resolve_labels' instrs =
  List.fold_left ~init:(0, []) ~f:(fun (addr, env) ->
      function
      | Ldef n -> (addr, (Lref n, Literal (addr)) :: env)
      | _ -> (addr+1, env))

let f (Asm.Prog (_, fundefs, main)) =
  let main = Asm.{ name= Id.L ("main"); args= []; fargs= []; ret= Type.Int;
                   body = main} in
  (compile_funs (main :: fundefs))
