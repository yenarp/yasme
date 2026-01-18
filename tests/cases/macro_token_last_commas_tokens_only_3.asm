macro t tokens l
	match l, a, b, c
		db 3
	else
		db 0
	end match
end macro

t a, b, c
