macro cap tokens t
  match t, "a,b,c"
    db 0x77
  else
    db 0x88
  end match
end macro

cap "a,b,c"
