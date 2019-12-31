open Printf
open MinCaml
open Insts

let max_stack_depth = 1000000

let debug_flg = ref false

let index_of element array =
  fst(List.find (fun (_,v) -> v=element)
        (List.mapi (fun idx v -> (idx,v)) (Array.to_list array)) )

let int_of_inst = function
  | Literal n -> n
  | Ldef lbl | Lref lbl -> failwith("unresolved "^lbl)
  | inst -> index_of inst insts
let string_of = function
  | Literal n -> Printf.sprintf "Literal %d" n
  | Ldef n    -> Printf.sprintf "Ldef %s" n
  | Lref n    -> Printf.sprintf "Lref %s" n
  | i         -> string_of_int(int_of_inst i)

(* operand stack

   We pair the stack pointer and the array of values.  Though the
   reference to the arry is not changed over the push/pop
   operations, those operations return a new pair just for
   convenience.  If we switch to the MinCaml implementation, we
   should decouple the pair.
*)

type value =
    Int' of int
  | Array' of value array

module Value = struct
  let (|+|) v1 v2 = match v1, v2 with
    | Int' i, Int' j -> Int' (i + j)
    | _ -> failwith "invalid value"

  let (|-|) v1 v2 = match v1, v2 with
    | Int' i, Int' j -> Int' (i - j)
    | _ -> failwith "invalid value"

  let (|*|) v1 v2 = match v1, v2 with
    | Int' i, Int' j -> Int' (i * j)
    | _ -> failwith "invalid value"

  let (|<|) v1 v2 = match v1, v2 with
    | Int' i, Int' j -> i < j
    | _ -> failwith "invalid value"

  let int_of_value = function
      Int' i -> i
    | _ -> failwith "array is not int"
  let array_of_value = function
    | Array' arr -> arr
    | Int' i -> failwith (sprintf "int %d is not array" i)

  let value_of_int i = Int' i

  let value_of_array arr = Array' arr
end

type stack = int * value array
let push : stack -> value -> stack =
  fun (sp,stack) v -> stack.(sp)<-v; (sp+1,stack)
let pop  : stack -> (value * stack) =
  fun (sp,stack) -> (stack.(sp-1), (sp-1, stack))
let take : stack -> int -> value =
  fun (sp,stack) n -> stack.(sp-n-1)
let drop : stack -> int -> stack =
  fun (sp,stack) n -> (sp-n,stack)
let frame_reset : stack -> int -> int -> int -> stack =
  fun (sp,stack) o l n ->
  let ret = stack.(sp-n-l-1) in (* save return address *)
  let old_base = sp - n - l - o - 1 in
  let new_base = sp - n in
  let rec loop i =
    if n=i then (stack.(old_base+n)<-ret; (old_base+n+1, stack))
    else (stack.(old_base+i)<-stack.(new_base+i);
          loop (i+1)) in
  loop 0
let make_stack () = (0, Array.make max_stack_depth (Int' 0))

(* let test_stack =(9,  [|1;2;3;4;5;6;7;8;9|])
 * let reset_result = frame_reset test_stack 2 2 3
 * let _ = Printf.printf "reset -> [%s]\n"
 *           (String.concat ";"
 *              (List.map string_of_int (Array.to_list (snd reset_result))))
 * let _ = assert  ((5, [|1;7;8;9;4;6;7;8;9|]) = reset_result) *)

(* fetch one integer from the code, and advance the program counter *)
let fetch code pc = (code.(pc), pc+1)

let code_at_pc code pc =
  if 0<=pc && pc<Array.length(code) then
    Printf.sprintf "code[%d..]=%d %d" pc code.(pc)
      (if pc+1<Array.length(code) then code.(pc+1) else -1)
  else Printf.sprintf "pc=%d" pc

let dump_stack (sp,stack) =
  let rec loop i = if i=sp then ""
    else (
      (match stack.(i) with
      | Int' i -> string_of_int i
      | Array' _ -> "array") ^";"^ (loop (i+1))
    ) in
  "["^(loop 0)^"]"

(* when the VM won't stop, you may turn on the following function to
   forcingly terminate after executing a certain amount of
   instructions *)
let checkpoint =
  if false
  then let counter = ref 5000 in
    fun () ->
      if !counter = 0
      then failwith "expired!"
      else counter := !counter - 1
  else fun () -> ()

let debug pc inst stack =
  if !debug_flg then
    Printf.eprintf "%d %s %s\n" (pc-1) (show_inst inst) (dump_stack stack)
  else ()

let rec interp code pc stack =
  checkpoint ();
  let open Value in
  if pc<0 then fst(pop stack) else begin
    let i,pc = fetch code pc in
    let inst = insts.(i) in
    debug pc inst stack;
    match inst with
    | UNIT ->
      interp code (pc + 1) stack
    | NOT ->
      let v,stack = pop stack in
      let stack =
        if int_of_value v = 0
        then push stack (Int' 1)
        else push stack (Int' 0)
      in
      interp code pc stack
    | ADD ->
      let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (v1 |+| v2) in
      interp  code pc stack
    | SUB -> let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (v1 |-| v2) in
      interp  code pc stack
    | MUL -> let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (v1 |*| v2) in
      interp  code pc stack
    | LT ->
      let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (if v1 |<| v2 then Int' 1 else Int' 0) in
      interp  code pc stack
    | CONST -> let c,pc = fetch code pc in
      let stack = push stack (value_of_int c) in
      interp  code pc stack
    | JUMP_IF_ZERO (* addr *) ->
      let addr,pc = fetch code pc in
      let v,stack = pop stack in
      (* interp  code (if v=0 then addr else pc) stack *)
      if int_of_value v = 0
      then interp code addr stack
      else interp code pc   stack
    | CALL (* addr argnum *) ->
      (* calling a function will create a new operand stack and lvars  *)
      let addr,pc = fetch code pc in
      let _,pc = fetch code pc  in
      let stack = push stack (value_of_int pc) in (* save return address *)
      (* (let (sp,s)=stack in
       *  if 2<sp then
       *    (Printf.printf "%d CALL %d [%d %d ...]\n" (pc-2) addr
       *       (s.(sp-2)) (s.(sp-3)))
       *  else ())
       * ; *)
      interp  code addr stack
    | RET (* n *) ->
      (* let pc0 = pc-1 in *)
      let n,pc = fetch code pc in
      let v,stack = pop stack in (* return value *)
      let pc,stack = pop stack in (* return address *)
      let stack = drop stack n in (* delete arguments *)
      let stack = push stack v in (* restore return value *)
      (* Printf.printf "%d RET with %d to %d\n" pc0 v pc; *)
      interp  code (int_of_value pc) stack
    | DUP ->
      let n,pc = fetch code pc in
      let stack = push stack (take stack n) in
      interp  code pc stack
    | HALT -> fst(pop stack)    (* just return the top value *)
    | FRAME_RESET (* n *) ->
      let o,pc = fetch code pc in
      let l,pc = fetch code pc in
      let n,pc = fetch code pc in
      let stack = frame_reset stack o l n in
      interp  code pc stack
    | POP1 ->
      let v,stack = pop stack in
      let _,stack = pop stack in
      let stack = push stack v in
      interp  code pc stack
    | JUMP (* addr *)->
      let n,_ = fetch code pc in
      interp code n stack
    | METHOD_ENTRY ->
      interp code pc stack
    | ARRAY_MAKE ->
      let init,stack = pop stack in
      let size,stack = pop stack in
      let stack =
        push stack
          (value_of_array
             (Array.make (int_of_value size) init)) in
      interp code pc stack
    | GET ->
      let n,stack = pop stack in
      let n = int_of_value n in
      let arr,stack = pop stack in
      let arr = array_of_value arr in
      let stack = push stack (arr.(n)) in
      interp code pc stack
    | PUT ->
      let i,stack = pop stack in
      let arr,stack = pop stack in
      let n,stack = pop stack in
      (array_of_value arr).(int_of_value i) <- n;
      let stack = push stack arr in
      interp code pc stack
    | PRINT_INT ->
      let n,stack = pop stack in
      print_int (int_of_value n);
      interp code pc stack
  end

(* run the given program by calling the function id 0 *)
type fundef_bin_t = int array
let run_bin : fundef_bin_t -> int = fun fundefs ->
  let open Value in
  let stack = (push (make_stack ()) (value_of_int (-987))) in
  int_of_value @@ interp fundefs 0 stack

(* convert the given program into binary, and then run *)
type fundef_asm_t = inst array
let run_asm : fundef_asm_t -> int = fun fundefs ->
  run_bin (Array.map int_of_inst fundefs)
