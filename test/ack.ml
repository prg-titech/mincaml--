let%tj rec ack x y =
  if x < 1 then y + 1 else
  if y < 1 then ack (x - 1) 1 else
  ack (x - 1) (ack x (y - 1)) in
print_int (ack 3 4)
