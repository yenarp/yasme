macro t tokens l
	match l, a
		db 1
	else
		db 0
	end match
end macro

t a
