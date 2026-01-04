macro m tokens t
  match missing, 1
    db 0x11
  end match
end macro

m 1
