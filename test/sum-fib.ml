let%mj rec fib n =
  if n <= 2 then 1 else
    fib (n-1) + fib (n-2) in
let%tj rec sum i =
  let m = fib 19 in
  if i <= 1 then 1 else
    m + sum (i-1) in
print_int (sum 15)
