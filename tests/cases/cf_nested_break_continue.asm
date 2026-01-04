for i = 0, 1
  db 0xa0

  for j = 0, 3
    if j == 1
      continue
    end if
    if j == 2
      break
    end if
    db (j + 1) * 0x10
  end for

  db 0xaf
end for
