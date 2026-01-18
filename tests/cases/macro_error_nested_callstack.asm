macro inner tokens t
	error "inner failed", t
	end error
end macro

macro outer tokens t
	inner t
end macro

outer 1 + 2

