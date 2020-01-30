let%mj rec fib n =
  if n <= 1 then 1 else
    fib (n-1) + fib (n-2) in
let%mj rec sum n =
  let m = fib 20 in
  if n <= 1 then m else
    m + sum (n - 1)
in print_int (sum 15)
