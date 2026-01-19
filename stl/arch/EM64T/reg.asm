macro get_reg_num ref num, ref size, tokens l
	; TODO xmmN mmN Xh

	RAX = 0b000
	RCX = 0b001
	RDX = 0b010
	RBX = 0b011
	RSP = 0b100
	RBP = 0b101
	RSI = 0b110
	RDI = 0b111

	num = 0xFF
	size = 0xFF

	; 000
	match l, rax
		num = RAX
		size = 8
	end match
	match l, eax
		num = RAX
		size = 4
	end match
	match l, ax
		num = RAX
		size = 2
	end match
	match l, al
		num = RAX
		size = 1
	end match

	; 001
	match l, rcx
		num = RCX
		size = 8
	end match
	match l, ecx
		num = RCX
		size = 4
	end match
	match l, cx
		num = RCX
		size = 2
	end match
	match l, cl
		num = RCX
		size = 1
	end match

	; 010
	match l, rdx
		num = RDX
		size = 8
	end match
	match l, edx
		num = RDX
		size = 4
	end match
	match l, dx
		num = RDX
		size = 2
	end match
	match l, dl
		num = RDX
		size = 1
	end match

	; 011
	match l, rbx
		num = RBX
		size = 8
	end match
	match l, ebx
		num = RBX
		size = 4
	end match
	match l, bx
		num = RBX
		size = 2
	end match
	match l, bl
		num = RBX
		size = 1
	end match

	; 100
	match l, rsp
		num = RSP
		size = 8
	end match
	match l, esp
		num = RSP
		size = 4
	end match
	match l, sp
		num = RSP
		size = 2
	end match
	match l, ah
		num = RSP
		size = 1
	end match

	; 101
	match l, rbp
		num = RBP
		size = 8
	end match
	match l, ebp
		num = RBP
		size = 4
	end match
	match l, bp
		num = RBP
		size = 2
	end match
	match l, ch
		num = RBP
		size = 1
	end match

	; 110
	match l, rsi
		num = RSI
		size = 8
	end match
	match l, esi
		num = RSI
		size = 4
	end match
	match l, si
		num = RSI
		size = 2
	end match
	match l, dh
		num = RSI
		size = 1
	end match

	; 111
	match l, rdi
		num = RDI
		size = 8
	end match
	match l, edi
		num = RDI
		size = 4
	end match
	match l, di
		num = RDI
		size = 2
	end match
	match l, bh
		num = RDI
		size = 1
	end match
end macro
