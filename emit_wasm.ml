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
let func_name (Id.L (x)) = "$" ^ x
let local_name x = "$" ^ x
let local_name_or_imm = function
  | V (x) -> Printf.sprintf "get_local %s" (local_name x)
  | C (i) -> Printf.sprintf "i32.const %d" i

let wat_type = function
  | Type.Float -> "f32"
  | _ -> "i32"

let func_sig oc = function
  | Type.Fun(tys, ret) ->
      List.iter
        (fun ty ->
           if ty <> Type.Unit then
             Printf.fprintf oc "(param %s)" (wat_type ty))
        tys;
      if ret <> Type.Unit then
        Printf.fprintf oc " (result %s)" (wat_type ret);
  | _ -> assert false

let rec g oc = function
  | Ans (e) -> gexp oc e
  | Let ((x, t), e, n) ->
    (* Calculation of `e` should leave a variable when t is not unit. *)
      gexp oc e;
      if t <> Type.Unit then
        begin
          Printf.fprintf oc
            "    set_local %s\n"
            (local_name x)
        end;
      g oc n
and gexp oc = function
  | Nop -> ()
  | Mov (x) ->
    Printf.fprintf oc "    i32.const %s\n" x;
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
  | IfEq (x, y, t1, t2) ->
    Printf.fprintf oc
      "  (if (i32.eq (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name_or_imm y);
    Printf.fprintf oc
      "  (then";
    g oc t1;
    Printf.fprintf oc
      "  )";
    Printf.fprintf oc
      "  (else";
    g oc t1;
    Printf.fprintf oc
      "  ))";
  | IfGE (x, y, t1, t2) ->
    Printf.fprintf oc
      "  (if (i32.gt_s (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name_or_imm y);
    Printf.fprintf oc
      "  (then";
    g oc t1;
    Printf.fprintf oc
      "  )";
    Printf.fprintf oc
      "  (else";
    g oc t1;
    Printf.fprintf oc
      "  ))";
  | IfLE (x, y, t1, t2) ->
    Printf.fprintf oc
      "  (if (i32.lt_s (%s) (%s))\n"
      (Printf.sprintf "get_local %s" (local_name x))
      (local_name_or_imm y);
    Printf.fprintf oc
      "  (then";
    g oc t1;
    Printf.fprintf oc
      "  )";
    Printf.fprintf oc
      "  (else";
    g oc t1;
    Printf.fprintf oc
      "  ))";
  | _ -> raise (Error "un implemented instruction")


and gv oc x =
  Printf.fprintf oc "    get_local %s\n" (local_name x)

and gvf oc x =
  Printf.fprintf oc "    get_local %s\n" (local_name x)

and gv_imm oc = function
  | V (x) -> Printf.fprintf oc "    get_local %s\n" (local_name x)
  | C (i) -> Printf.fprintf oc "    i32.const %d\n" i
