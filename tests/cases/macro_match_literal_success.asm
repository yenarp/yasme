macro m tokens t
  match t, 1+2
    db 0x11
  else
    db 0x22
  end match
end macro

m 1+2
