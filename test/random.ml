let rec gen_random max =
  let last = 47 in
  let im = 13998 in
  let ia = 3877 in
  let ic = 29573 in
  let last = ((last * ia) + ic) mod im in
  (max * last) / im in
let rec loop n i acc =
  if i < n then
    let x = gen_random i in
    loop n (i+1) (acc+x)
  else acc
in
print_int (loop 100000000 1 1)
