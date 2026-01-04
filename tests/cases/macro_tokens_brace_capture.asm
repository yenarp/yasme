macro cap tokens t
  match t, ...
    db 0x55
  else
    db 0x66
  end match
end macro

cap {1,2}
