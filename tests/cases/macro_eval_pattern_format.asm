macro wrap_eval tokens t
	eval tmp, ({t})
	eval out, tmp
	eval *out2, ({t})
	db out
	db out2
end macro

wrap_eval 1+2*3
