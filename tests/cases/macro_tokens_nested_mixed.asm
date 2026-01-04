macro cap tokens t
  match t, [foo(1,2),bar]
    db 0xbb
  else
    db 0xcc
  end match
end macro

cap [foo(1,2),bar]
