macro bad_form tokens t
	error "unsupported operand form", t
		ref "argument here", t
	end error
end macro

bad_form [eax]

