macro m tokens t
	eval tmp, ({t} + 1)
	match tmp, ({x} + 1)
		eval out, x
		db out
	else
		db 0
	end match
end macro

m 2*3
