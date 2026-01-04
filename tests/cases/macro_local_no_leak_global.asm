x = 0x33
macro m tokens t
  local x
  eval x, t
  db x
end macro

m 1
db x
