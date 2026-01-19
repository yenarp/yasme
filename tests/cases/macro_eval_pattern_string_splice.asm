macro m
  local s
  s = "hi"
  eval *out, ({s})
  db out
end macro

m
