macro bad tokens t
	error "outer message", t
		help 456
	end error
end macro

bad 1

