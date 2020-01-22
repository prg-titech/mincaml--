let%mj rec ack x y =
  if x <= 0 then y + 1 else
  if y <= 0 then ack (x - 1) 1 else
  ack (x - 1) (ack x (y - 1)) in
let%tj rec sum i n =
  let x = ack 2 5 in
  if i <= 1 then n else
    sum (i - 1) (n + x) in
print_int (sum 1000 0)
