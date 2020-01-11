let rec fact x =
  if x <= 1 then 1 else
    x * fact (x-1) in
let s = get_current_micros () in
let _ = fact 10000 in
let e = get_current_micros () in
print_int (e - s)
