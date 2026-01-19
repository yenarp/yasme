macro m
  local x
  x = 1 + 2
  eval *out, ({x} * 3)
  db out
end macro

m
