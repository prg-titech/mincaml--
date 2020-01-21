let rec loop1 flags i k =
  if k <= 8192 then
    (flags.(k) <- 0;
     loop1 flags i (k+i))
  else flags in

let rec sieve flags i =
  if i <= 8192 then
    if flags.(i) = 1 then
      let flags = loop1 flags i (i+1) in
      sieve flags (i + 1)
    else
      sieve flags (i + 1)
  else
    flags in

let flags = Array.make 8193 1 in
let _ = sieve flags 2 in
print_int (flags.(17))
