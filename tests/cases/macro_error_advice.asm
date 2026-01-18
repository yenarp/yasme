macro imm8 tokens t
	error "imm8: argument does not fit", t
		help "expected: 0..255"
		suggestion "try: imm8 123"
	end error
end macro

imm8 999
