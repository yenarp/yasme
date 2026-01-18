macro fail tokens t
	error "fail: boom", t
	end error
end macro

fail 1 + 2
db 0x00

