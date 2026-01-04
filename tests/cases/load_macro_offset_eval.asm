macro load_at tokens t
  local off
  eval off, t
  load db x, s, off
  db x
end macro

virtual s
  db 0x10, 0x20, 0x30
end virtual

load_at 1
