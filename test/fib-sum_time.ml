let%tj rec sum n =
  if n <= 1 then 1 else
    n + sum (n - 1) in
let%mj rec fib n =
  if n <= 1 then sum 100 else
    fib (n-1) + fib (n-2) in
let rec loop i =
  if i = 0 then () else
    let s = get_current_micros () in
    let r = fib 11 in
    let e = get_current_micros () in
    print_int (e - s); print_newline ();
    loop (i-1)
in loop 100
