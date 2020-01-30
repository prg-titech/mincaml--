let%mj rec fib n =
  if n < 2 then 1 else
    fib (n-1) + fib (n-2) in
let%tj rec sum i =
  if i < 0 then 1 else
    fib i + sum (i-1) in
let rec loop i =
  if i < 1 then ()
  else
    let s = get_current_micros () in
    let _ = sum 22 in
    let e = get_current_micros () in
    print_int (e - s); print_newline ();
    loop (i-1)
in loop 150
