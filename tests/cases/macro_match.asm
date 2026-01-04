macro choose tokens t
  match t, 1
    db 0x11
  else
    db 0x22
  end match
end macro

choose 1
choose 2
