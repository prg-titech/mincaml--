let%mj rec fib n =
  if n < 2 then 1 else
    fib (n-1) + fib (n-2) in
let%tj rec f i n =
  if i < 0 then n else
    f (i-1) (n + (fib i)) in
print_int (f 10 0)
