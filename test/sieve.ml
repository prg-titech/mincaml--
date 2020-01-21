let rec loop1 flags k i =
  if k < 4000 then  (* MJ k <= 4000 *)
    (flags.(k) <- (-1);
     loop1 flags (k+i) i)
  else flags in
let rec sieve flags i =
  if i < 4000 then (* MJ k <= 4000 *)
    if flags.(i) = 1 then
      let _ = loop1 flags (i+1) i in
      sieve flags (i+1)
    else
      sieve flags (i+1)
  else
    flags in
let flags = Array.make 4000 1 in
let _ = sieve flags 2 in
()
