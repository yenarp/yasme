include 'util.asm'
include 'reg.asm'
include 'emit.asm'

macro mov_reg_imm tokens t
	match t, {dst}, {imm}
		local dstnum
		local size
		get_reg_num dstnum, size, dst

		emit_mov_imm dstnum, size
		emit_imm imm, size
	end match
end macro

macro mov_reg_reg tokens t
	match t, {dst}, {src}
		local dstnum
		local dstsize
		get_reg_num dstnum, dstsize, dst
		local srcnum
		local srcsize
		get_reg_num srcnum, srcsize, src

		evalwrap modrm, 0b11<<6|(dstnum)|(srcnum<<3)

		if dstsize - srcsize
			; error "Invalid operand sizes", t
			; end error
		end if

		emit_mov_mr srcsize

		db modrm
	
	end match
end macro

macro mov_deref_reg_reg tokens t
	match t, [{dst}], {src}

		local dstnum
		local dstsize
		get_reg_num dstnum, dstsize, dst
		local srcnum
		local srcsize
		get_reg_num srcnum, srcsize, src

		evalwrap modrm, 0b00<<6|(dstnum)|(srcnum<<3)

		if dstsize - srcsize
			; error "Invalid operand sizes", t
			; end error
		end if

		emit_mov_mr srcsize

		db modrm
	
	end match
end macro

macro mov_reg_deref_reg tokens t
	match t, {dst}, [{src}]

		local dstnum
		local dstsize
		get_reg_num dstnum, dstsize, dst
		local srcnum
		local srcsize
		get_reg_num srcnum, srcsize, src

		evalwrap modrm, 0b00<<6|(dstnum<<3)|(srcnum)

		if dstsize - srcsize
			; error "Invalid operand sizes", t
			; end error
		end if

		emit_mov_rm srcsize

		db modrm
	
	end match
end macro

macro mov tokens t
	match t, [{dst}], [{src}]
		error "Invalid operands", t
		end error
	else
		match t, {dst}, [{src}]
			mov_reg_deref_reg t
		else
			match t, [{dst}], {src}
				mov_deref_reg_reg t
			else
				match t, {dst}, {src}
					local srcnum
					local srcsize
					get_reg_num srcnum, srcsize, src

					if srcnum == 0xFF ; immediate
						mov_reg_imm t
					else
						mov_reg_reg t
					end if

				end match
			end match
		end match
	end match
end macro
