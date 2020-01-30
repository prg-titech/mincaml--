let%tj rec sum i n =
  if i <= 0 then n else
    sum (i - 1) (n + i) in
let%tj rec tak x y z =
  if x <= y then sum 10 0 else
    tak (tak (x-1) y z)
      (tak (y-1) z x)
      (tak (z-1) x y)
in
print_int (tak 14 8 3)
