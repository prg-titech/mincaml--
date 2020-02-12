let%tj rec sum i =
  if i <= 1 then i else
    i + sum (i - 1)  in
let%mj rec tak x y z =
  if x <= y then sum 100 else
    tak (tak (x-1) y z)
      (tak (y-1) z x)
      (tak (z-1) x y)
in
print_int (tak 13 6 3)
