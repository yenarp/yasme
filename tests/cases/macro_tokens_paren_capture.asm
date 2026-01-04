macro cap tokens t
  match t, foo(1,2,bar(3,4))
    db 0x11
  else
    db 0x22
  end match
end macro

cap foo(1,2,bar(3,4))
