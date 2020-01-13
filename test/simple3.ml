let%tj rec sum n acc =
  if n < 2 then acc else
    sum (n-1) (n+acc) in
let%mj rec fib n =
  if n < 2 then sum 10000 0 else
    fib (n-1) + fib (n-2) in
print_int (fib 20)
