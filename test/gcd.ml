let%tj rec gcd m n =
  if m < 1 then n else
  if m < n + 1 then gcd m (n - m) else
  gcd n (m - n) in
print_int (gcd 21600 337500)
