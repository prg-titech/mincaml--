;;
let%mj rec fib n = if n < 1 then 1 else fib (n - 1) + fib (n - 2) in
let%tj rec sum n = if n < 1 then 1 else n + sum (n - 1) in
print_int (sum 1000 + fib 20)
