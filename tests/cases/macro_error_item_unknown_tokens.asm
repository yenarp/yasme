macro bad tokens t
	error "item unknown", t
		note "this note wants a missing span", nope
	end error
end macro

bad 1

