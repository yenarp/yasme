macro emit_name name n
  db n
end macro

foo_bar = 0x66
emit_name foo_#bar
