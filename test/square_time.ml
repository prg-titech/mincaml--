let rec square i x =
  if i < 1 then 0 else
    x + square (i-1) x in
let s = get_current_micros () in
let _ = square 10000 10000 in
let e = get_current_micros () in
print_int (e - s)
