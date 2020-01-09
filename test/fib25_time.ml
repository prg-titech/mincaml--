let rec fib n =
  if n < 2 then 1 else
  fib (n - 1) + fib (n - 2) in
let s = get_current_micros () in
let _ = fib 25 in
let e = get_current_micros () in
print_int (e - s)
