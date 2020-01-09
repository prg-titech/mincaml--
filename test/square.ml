let%tj rec square i x acc =
  if i < 1 then acc else
    square (i-1) x (acc + x) in
print_int (square 10 10 0)
