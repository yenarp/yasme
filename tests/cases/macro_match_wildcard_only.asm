macro m tokens t
  match t, _
    db 0x88
  else
    db 0x99
  end match
end macro

m x
