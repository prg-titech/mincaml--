let%mj rec tak x y z =
  if x <= y then z else
    tak (tak (x-1) y z) (tak(y-1) z x) (tak(z-1) x y)
in
let%tj rec sum n =
  let m = tak 12 6 4 in
  if i <= 1 then m else
    m + sum (i-1) n
in
let _ = sum 100 in
()
