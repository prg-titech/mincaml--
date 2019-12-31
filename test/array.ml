let rec simple n m =
  Array.make n m
in
let arr = simple 2 3 in
arr.(0) <- arr.(0) + 5;
print_int (arr.(0) + arr.(1))
