let rec fact x =
  if x <= 1 then 1 else
    x * fact (x-1) in
let rec loop i =
  if i = 0 then () else
  let s = get_current_micros () in
  let r = fact 10000 in
  let e = get_current_micros () in
  print_int (e - s); print_newline ();
  loop (i-1) in
loop 100
