macro m tokens t
  match t, {1}
    db 0x11
  end match
end macro

m 1
