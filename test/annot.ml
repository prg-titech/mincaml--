let%tj rec f x =
  if x = 0 then x else (-x)
in print_int (f 10)
