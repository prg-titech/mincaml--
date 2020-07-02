let%tj rec square i x =
  if i <= 1 then x else
    let j = i - 1 in
    x + square j x
in
print_int (square 5000 5000)
