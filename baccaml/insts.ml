type inst =
  | UNIT
  | ADD
  | SUB
  | MUL
  | NEG
  | NOT
  | LT
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

let pp_insts_counter = ref 1
let rec pp_insts insts =
  match insts with
  | [] -> ()
  | hd :: tl ->
    begin
      match hd with
      | CONST | DUP | JUMP_IF_ZERO | JUMP ->
        print_int !pp_insts_counter; print_string "\t";
        print_string (show_inst hd); print_string " ";
        incr pp_insts_counter;
        pp_insts tl
      | CALL ->
        print_int !pp_insts_counter; print_string "\t";
        print_string (show_inst hd); print_string " ";
        let hd,tl = List.hd tl,List.tl tl in
        print_string (show_inst hd); print_string " ";
        incr pp_insts_counter;
        pp_insts tl
      | Literal n ->
        print_string (show_inst hd); print_newline ();
        incr pp_insts_counter;
        pp_insts tl
      | _ ->
        print_int !pp_insts_counter; print_string "\t";
        print_string (show_inst hd); print_newline ();
        incr pp_insts_counter;
        pp_insts tl
    end;
