macro u rhs, tokens lhs
	match lhs, x,
		db rhs + 10
	else
		db rhs
	end match
end macro

u 7, x,
