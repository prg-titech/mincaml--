let rec sum i n =
  if i <= 0 then n else
    sum (i - 1) (n + i) in
let rec tak x y z =
  if x <= y then sum 10 0 else
    tak (tak (x-1) y z)
      (tak (y-1) z x)
      (tak (z-1) x y)
in
let rec loop i =
  if i = 0 then () else
    let s = get_current_micros () in
    let r = (tak 14 8 3) in
    let e = get_current_micros () in
    print_int (e - s); print_newline ();
    loop (i-1)
in loop 101
