let%tj rec loop1 flags k i =
  if k <= 8192 then  (* MJ k <= 4000 *)
    (flags.(k) <- (-1);
     loop1 flags (k+i) i)
  else flags in
let%tj rec sieve flags i =
  if i <= 8192 then (* MJ k <= 4000 *)
    if (-1) < flags.(i) then
      let _ = loop1 flags (i+i) i in
      sieve flags (i+1)
    else
      sieve flags (i+1)
  else
    flags in
let flags = Array.make 8192 1 in
flags.(0) <- (-1); flags.(1) <- (-1);
let _ = sieve flags 1 in
print_int (flags.(23));
()
