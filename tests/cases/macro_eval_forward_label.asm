macro calc tokens t
  eval out, t
end macro

calc foo + 1
db 0x00
foo:
db 0x00
db out
