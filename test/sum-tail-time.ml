let rec sum acc x =
  if x <= 0 then acc else
  sum (acc + x) (x - 1) in
let s = get_current_micros () in
let _ = sum 0 30000 in
let e = get_current_micros () in
print_int (e - s)
