macro m tokens t
	match t, {a} + {b}
		eval *out, ({a} * {b})
		db out
	else
		db 0
	end match
end macro

m 2+3
