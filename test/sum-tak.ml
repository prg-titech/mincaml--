let%mj rec tak x y z =
  if x <= y then z else
    tak (tak (x-1) y z) (tak(y-1) z x) (tak(z-1) x y)
in
let%tj rec sum i n =
  if i <= 1 then n else
    let m = tak 12 6 4 in
    sum (i-1) (n + m)
in
let _ = sum 100 0 in
()
