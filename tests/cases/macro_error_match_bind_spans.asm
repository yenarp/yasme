macro no_add tokens t
	match t, {lhs} + {rhs}
		error "no additions allowed", lhs
			ref "lhs", lhs
			ref "rhs", rhs
			help "use a single value instead"
		end error
	else
		db 0x00
	end match
end macro

no_add 1 + 2

