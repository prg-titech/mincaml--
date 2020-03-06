module C = Closure
module V = Virtual
module A = Asm

let rec clsr_of_virt = function
  | A.Let ((x, typ), e, t) ->
    C.Let ((x, typ), clsr_of_virt' e, clsr_of_virt t)
  | A.Ans (e) -> clsr_of_virt' e

and clsr_of_virt' = function
  | A.Nop -> C.Unit
  | A.Set (i) -> C.Int (i)
  | A.SetL (l) -> C.Unit
  | A.Mov (v) -> C.Var (v)
  | A.Add (x, A.V y) -> C.Add (x, y)
  | A.Sub (x, A.V y) -> C.Sub (x, y)
  | A.Div (x, A.V y) -> C.Div (x, y)
  | A.Mod (x, A.V y) -> C.Mod (x, y)
  | A.Mul (x, A.V y) -> C.Mul (x, y)
