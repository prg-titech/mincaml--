let rec fib n =
  if n < 2 then n else
  fib (n - 1) + fib (n - 2) in
let s = get_current_micros () in
print_int (fib 30); print_newline ();
let e = get_current_micros () in
print_int (e - s)
