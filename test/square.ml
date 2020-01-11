let rec square i x =
  if i < 2 then x else
    x + square (i-1) x
in
print_int (square 5000 5000)
