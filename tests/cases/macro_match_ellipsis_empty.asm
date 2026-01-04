macro m tokens t
  match t, a ... b
    db 0x55
  else
    db 0x66
  end match
end macro

m a ... b
