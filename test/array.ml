let rec ary i n xs ys =
  if n < i
  then ys
  else (
    ys.(i) <- xs.(i) + ys.(i);
    ary (i + 1) n xs ys)
in
let n = 5000 in
let xs = Array.make n 1 in
let ys = Array.make n 2 in
let _ = ary 0 n xs ys in
print_int ys.(1)
