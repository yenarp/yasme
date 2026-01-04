macro m tokens t
  match t, {a} + ...
    eval out, a
    db out
  else
    db 0x00
  end match
end macro

m 1 + 2 + 3
