macro inc ref x
  x = x + 1
end macro

macro inc2 ref x
  inc x
  inc x
end macro

val = 1
inc2 val
db val
