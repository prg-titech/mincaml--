let%tj rec sum n =
  if n <= 1 then 1 else
    n + sum (n - 1) in
let%mj rec fib n =
  if n <= 0 then sum 100 else
    fib (n-1) + fib (n-2) in
print_int (fib 20)
