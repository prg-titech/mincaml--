let rec swap xs idx max =
  if max - 1 <= idx
  then ()
  else (
    let l = xs.(idx) in
    let r = xs.(idx + 1) in
    if l <= r
    then swap xs (idx + 1) max
    else (
      xs.(idx) <- r;
      xs.(idx + 1) <- l;
      swap xs (idx + 1) max))
in
let rec bubble_sort xs idx len =
  if idx >= len - 1
  then xs
  else (
    swap xs 0 (len - idx);
    bubble_sort xs (idx + 1) len)
in
let a = Array.make 3 0 in
a.(0) <- 3;
a.(1) <- 1;
a.(2) <- 2;
let a = bubble_sort a 0 3 in
print_int (a.(0)); print_newline ();
print_int (a.(1)); print_newline ();
print_int (a.(2)); print_newline ()
