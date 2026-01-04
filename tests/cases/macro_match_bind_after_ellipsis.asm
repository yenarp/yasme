macro m tokens t
  match t, ... + {b}
    eval out, b
    db out
  else
    db 0x00
  end match
end macro

m 1 + 2
