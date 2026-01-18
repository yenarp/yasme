
macro use tokens arch
	match arch, 8086
		dw 8086
	else match arch, x86
		dw 80186
	else match arch, EM64T
		db 64
	else
		error "invalid arch", arch
			help "valid: 8086, x86, EM64T"
		end error
	end match
end macro
