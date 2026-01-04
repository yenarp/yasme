macro m tokens t
  match t, a ... b
    db 0x66
  else
    db 0x77
  end match
end macro

m a + b
