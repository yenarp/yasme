macro t tokens l
	match l, a, b
		db 2
	else
		db 0
	end match
end macro

t a, b
