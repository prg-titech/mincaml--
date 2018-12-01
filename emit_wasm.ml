open Asm

exception Error of string

(* super-tenuki global variables *)
(* let fentries = ref [] *)

let rec findi f = findi' f 0
and findi' f i = function
  | [] -> None
  | x::xs ->
    if f x then Some i
    else findi' f (i+1) xs

(* WAT style of function name. *)
let without_suffix x = String.split_on_char '.' x |> List.hd
let without_suffix_idl (Id.L (x)) = String.split_on_char '.' x |> List.hd
let local_name x = "$" ^ (without_suffix x)
let func_name (Id.L (x)) = local_name x
let local_name_or_imm = function
  | V (x) -> Printf.sprintf "get_local %s" (local_name x)
  | C (i) -> Printf.sprintf "i32.const %d" i

let wat_type = function
  | Type.Float -> "f32"
  | _ -> "i32"

let func_sig oc = function
  | Type.Fun (tys, ret) ->
    List.iter
      (fun ty ->
         if ty <> Type.Unit then
           Printf.fprintf oc "(param %s)" (wat_type ty))
      tys;
    if ret <> Type.Unit then
      Printf.fprintf oc " (result %s)" (wat_type ret);
  | _ -> assert false

let rec g oc = function
  | Ans (e) ->
    gexp oc e
  | Let ((x, t), e, n) ->
    (* Calculation of `e` should leave a variable when t is not unit. *)
      gexp oc e;
      if t <> Type.Unit then
        begin
          Printf.fprintf oc "    set_local %s\n" (local_name x)
        end;
      g oc n;
and gexp oc = function
  | Nop -> ()
  | Mov (x) ->
    Printf.fprintf oc "    get_local %s\n" (local_name x);
  | Set (i) ->
    Printf.fprintf oc "    i32.const %d\n" i;
  | Add (x, y) ->
    gv oc x;
    gv_imm oc y;
    Printf.fprintf oc "    i32.add\n";
  | Sub (x, y) ->
    gv oc x;
    gv_imm oc y;
    Printf.fprintf oc "    i32.sub\n";
  | FAddD (x, y) ->
    gvf oc x;
    gvf oc y;
    Printf.fprintf oc "    f32.add\n";
  | FSubD (x, y) ->
    gvf oc x;
    gvf oc y;
    Printf.fprintf oc "    f32.sub\n";
  | FMulD (x, y) ->
    gvf oc x;
    gvf oc y;
    Printf.fprintf oc "    f32.mul\n";
  | FDivD (x, y) ->
    gvf oc x;
    gvf oc y;
    Printf.fprintf oc "    f32.div_s\n";
  | Ld (x, y, z) ->
    Printf.fprintf oc "    get_local %s\n" (local_name x);
    begin match y with
      | V (x) ->
        Printf.fprintf oc "    get_local %s\n" (local_name x);
        Printf.fprintf oc "    i32.const %d\n" z;
        Printf.fprintf oc "    i32.mul\n";
        Printf.fprintf oc "    i32.load align=4\n";
      | C (i) ->
        Printf.fprintf oc "    i32.load offset=%d alig=4\n" (i * z);
    end
  | St (w, x, y, z) ->
    Printf.fprintf oc "    get_local %s\n" (local_name x);
    begin
      match y with
      | V (x) ->
        Printf.fprintf oc "    get_local %s\n" (local_name x);
        Printf.fprintf oc "    i32.const %d\n" z;
        Printf.fprintf oc "    i32.mul\n";
        Printf.fprintf oc "    get_local %s\n" (local_name w);
        Printf.fprintf oc "    i32.sotre align=4\n";
      | C (i) ->
        Printf.fprintf oc "    get_local %s\n" (local_name w);
        Printf.fprintf oc "    i32.load offset=%d alig=4\n" (i * z);
    end
  | IfEq (x, y, t1, t2) ->
    Printf.fprintf oc
      "  (if (result i32) (i32.eq (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name_or_imm y);
    Printf.fprintf oc
      "  (then\n";
    g oc t1;
    Printf.fprintf oc
      "  )\n";
    Printf.fprintf oc
      "  (else\n";
    g oc t2;
    Printf.fprintf oc
      "  ))\n";
  | IfGE (x, y, t1, t2) ->
    Printf.fprintf oc
      "  (if (result i32) (i32.gt_s (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name_or_imm y);
    Printf.fprintf oc
      "  (then\n";
    g oc t1;
    Printf.fprintf oc
      "  )\n";
    Printf.fprintf oc
      "  (else\n";
    g oc t2;
    Printf.fprintf oc
      "  ))\n";
  | IfLE (x, y, t1, t2) ->
    Printf.fprintf oc
      "  (if (result i32) (i32.lt_s (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name_or_imm y);
    Printf.fprintf oc
      "  (then\n";
    g oc t1;
    Printf.fprintf oc
      "  )\n";
    Printf.fprintf oc
      "  (else\n";
    g oc t2;
    Printf.fprintf oc
      "  ))\n";
  | IfFEq (x, y, t1, t2) ->
    Printf.fprintf oc
      "  (if (result f32) (f32.eq (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name y);
    Printf.fprintf oc
      "  (then\n";
    g oc t1;
    Printf.fprintf oc
      "  )\n";
    Printf.fprintf oc
      "  (else\n";
    g oc t2;
    Printf.fprintf oc
      "  ))\n";
  | IfFLE (x, y, t1, t2) ->
    Printf.fprintf oc
      "  (if (result f32) (i32.lt_s (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name y);
    Printf.fprintf oc
      "  (then\n";
    g oc t1;
    Printf.fprintf oc
      "  )\n";
    Printf.fprintf oc
      "  (else\n";
    g oc t2;
    Printf.fprintf oc
      "  ))\n";
  | CallDir (f, args, fargs) ->
    List.iter (gv oc)  args;
    List.iter (gvf oc) fargs;
    Printf.fprintf oc "    call %s\n" (func_name f);
  | CallCls (f, args, fargs) ->
    Printf.fprintf oc "    TODO  ;; CallCls is not implemented"
  | _ -> raise (Error "unimplemented instruction")


and gv oc x =
  Printf.fprintf oc "    get_local %s\n" (local_name x)

and gvf oc x =
  Printf.fprintf oc "    get_local %s\n" (local_name x)

and gv_imm oc = function
  | V (x) -> Printf.fprintf oc "    get_local %s\n" (local_name x)
  | C (i) -> Printf.fprintf oc "    i32.const %d\n" i


let h oc { name; args; fargs; body = e; ret } =
  (* Emit a function definition. *)
  Printf.fprintf oc "  (func %s" (func_name name);
  Printf.fprintf oc "  (export \"%s\")" (without_suffix_idl name);
  (* Declare function signature. *)
  List.iter
    (fun x ->
       Printf.fprintf oc " (param %s %s)"
         (local_name x)
         (wat_type Type.Int))
    args;
  List.iter
    (fun x ->
       Printf.fprintf oc " (param %s %s) "
         (local_name x)
         (wat_type Type.Float))
    fargs;
  if ret <> Type.Unit then
    Printf.fprintf oc " (result %s)"
      (wat_type ret);
  (* Declare local variables. *)
  Printf.fprintf oc "\n   ";
  List.iter
    (fun (x, t) ->
       Printf.fprintf oc " (local %s %s)"
         (local_name x)
         (wat_type t))
    (localvs e);
  (* Body of function. *)
  Printf.fprintf oc "\n";
  g oc e;
  Printf.fprintf oc ")\n"


let f oc (Prog (ftable, fundefs, main)) =
  Format.eprintf "generating assembly...@.";
  (* start module *)
  Printf.fprintf oc "(module\n";
  Printf.fprintf oc "  (func $min_caml_print_int (import \"imports\" \"log\") (param i32))\n";
  (* main *)
  let mainfun =
    { name = Id.L ("main"); args = []; fargs = []; body = main; ret = Type.Unit }
  in
  List.iter (h oc) (fundefs @ [mainfun]);
  (* end module *)
  Printf.fprintf oc ")";
