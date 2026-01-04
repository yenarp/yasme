macro calc tokens t
  local tmp
  eval tmp, t
  db tmp
end macro

calc 1 + 2 * 3
