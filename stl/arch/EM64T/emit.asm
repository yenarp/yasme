macro emit_operand_size_override
	db 0x66
end macro

macro emit_rex_w
	db 0x48
end macro

macro emit_mov_mr size
	if size == 1
		db 0x88
	end if
	if size == 2
		emit_operand_size_override
		db 0x89
	end if
	if size == 4
		db 0x89
	end if
	if size == 8
		emit_rex_w
		db 0x89
	end if
end macro

macro emit_mov_rm size
	if size == 1
		db 0x8A
	end if
	if size == 2
		emit_operand_size_override
		db 0x8B
	end if
	if size == 4
		db 0x8B
	end if
	if size == 8
		emit_rex_w
		db 0x8B
	end if
end macro
