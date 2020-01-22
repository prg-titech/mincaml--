let rec ack x y =
  if x < 1 then y + 1 else
  if y < 1 then ack (x - 1) 1 else
    ack (x - 1) (ack x (y - 1)) in
let s = get_current_micros () in
let _ = ack 3 5 in
let e = get_current_micros () in
print_int (e-s)
