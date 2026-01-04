macro m tokens t
  match t, {a}
    match a, _
      db 0x90
    else
      db 0x91
    end match
  else
    db 0x92
  end match
end macro

m 7
