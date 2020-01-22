let%mj rec ack x y =
  if x = 0 then y + 1 else
  if y = 0 then ack (x - 1) 1 else
  ack (x-1) (ack x (y-1)) in

let%mj rec sum i n =
  if i <= 1 then n else
    let x = ack 3 9 in
    sum (i-1) (n + x) in
print_int (sum 10 0)
