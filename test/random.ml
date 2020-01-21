let rec loop max i ran =
  let im = 13998 in
  let ia = 3877 in
  let ic = 29573 in
  if i <= 2 then 0 else
    let last = ran.(0) in
    let last = ((last * ia) + ic) mod im in
    let new_last = (max * last) / im in
    ran.(0) <- new_last;
    loop max (i-1) ran
in
let n = 1000 in
let max = 100 in
let ran = Array.make 42 2 in
let _ = (loop max n ran) in
()
