type inst =
  | UNIT
  | ADD
  | SUB
  | MUL
  | NEG
  | NOT
  | LT
  | EQ
  | JUMP_IF_ZERO
  | JUMP
  | CALL
  | RET
  | HALT
  | DUP
  | POP1
  | CONST
  | GET
  | PUT
  | ARRAY_MAKE
  | FRAME_RESET
  | PRINT_INT
  | PRINT_NEWLINE
  | METHOD_ENTRY
  | Literal of int
  | Lref of string
  | Ldef of string
[@@deriving show]

let insts = [|
  UNIT;
  ADD;
  SUB;
  MUL;
  NEG;
  NOT;
  LT;
  EQ;
  JUMP_IF_ZERO;
  JUMP;
  CALL;
  RET;
  HALT;
  DUP;
  POP1;
  CONST;
  GET;
  PUT;
  ARRAY_MAKE;
  FRAME_RESET;
  PRINT_INT;
  PRINT_NEWLINE;
  METHOD_ENTRY
|]

let pp_insts_counter = ref 0
let pp_pc () =
  print_int !pp_insts_counter;
  print_string "\t";
  incr pp_insts_counter

let rec pp_insts ?(i=0) insts =
  match insts with
  | [] -> ()
  | hd :: tl ->
    begin
      match hd with
      | CONST | DUP
      | JUMP | JUMP_IF_ZERO
      | RET ->
        pp_pc ();
        print_string (show_inst hd); print_string " ";
        pp_insts ~i:0 tl
      | CALL ->
        pp_pc ();
        print_string (show_inst hd); print_string " ";
        pp_insts ~i:1 tl
      | Literal n ->
        print_string "\t"; print_string (show_inst hd);
        if i = 0 then print_newline ()
        else print_string "\t";
        incr pp_insts_counter;
        pp_insts ~i:(i-1) tl
      | _ ->
        pp_pc ();
        print_string (show_inst hd); print_newline ();
        pp_insts ~i:0 tl
    end;
