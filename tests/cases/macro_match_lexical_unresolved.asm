macro m tokens t
  match t, foo + 1
    db 0x77
  else
    db 0x88
  end match
end macro

m foo + 1
