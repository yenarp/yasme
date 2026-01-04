macro calc tokens t
  local out
  eval out, t
  db out
end macro

calc (1 + 2) * 3
