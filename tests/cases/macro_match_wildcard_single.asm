macro m tokens t
  match t, _ + 2
    db 0x33
  else
    db 0x44
  end match
end macro

m 1+2
