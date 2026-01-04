macro cap tokens t
  match t, foo(1,2)
    db 0x99
  else
    db 0xaa
  end match
end macro

cap   foo ( 1 , 2 )
