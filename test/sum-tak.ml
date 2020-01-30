let%mj rec tak x y z =
  if x <= y then y else
  tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
in
let%tj rec sum i =
  let x = (tak 13 8 6) in
  if i <= 0 then x else
    x + sum (i - 1) in
print_int (sum 50)
