macro calc tokens t
  local out
  eval out, t
  db out
end macro

calc 2 + 3
