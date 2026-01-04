virtual s
  db 0x01, 0x02
end virtual

load dw x, s, 0
db x & 0xff
db (x >> 8) & 0xff
