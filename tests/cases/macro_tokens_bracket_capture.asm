macro cap tokens t
  match t, [a,b+1]
    db 0x33
  else
    db 0x44
  end match
end macro

cap [a,b+1]
