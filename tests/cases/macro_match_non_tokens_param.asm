macro m name n
  match n, foo
    db 0x11
  end match
end macro

m foo
