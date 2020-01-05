let rec swap xs idx max =
  if max < idx + 1
  then xs
  else (
    let l = xs.(idx) in
    let r = xs.(idx + 1) in
    if l < r + 1
    then swap xs (idx + 1) max
    else (
      xs.(idx) <- r;
      xs.(idx + 1) <- l;
      swap xs (idx + 1) max))
in
let rec bubble_sort xs idx len =
  if len - 1 < idx
  then xs
  else (
    let xs = swap xs 0 (len - idx) in
    bubble_sort xs (idx + 1) len)
in
let a = Array.make 3 0 in
a.(0) <- 3;
a.(1) <- 1;
a.(2) <- 2;
let a = bubble_sort a 0 3 in
print_int (a.(2))
