macro inner tokens t
  local x
  eval x, t
  db x
end macro

macro outer tokens t
  local x
  eval x, t
  inner 2
  db x
end macro

outer 1
