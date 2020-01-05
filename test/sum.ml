let rec sum x =
  if x < 1 then 0 else
  sum (x - 1) + x in
print_int (sum 10000)
