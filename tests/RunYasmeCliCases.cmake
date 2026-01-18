if(NOT DEFINED WORKDIR)
	set(WORKDIR "${CMAKE_CURRENT_LIST_DIR}")
endif()

get_filename_component(out_dir "${OUTFILE}" DIRECTORY)
file(MAKE_DIRECTORY "${out_dir}")

execute_process(
	COMMAND "${CMAKE_COMMAND}" -E env "YASME_INCLUDE=${INCLUDE_ENV}"
		"${YASME_CLI}"
			-o "${OUTFILE}"
			-i "include 'prelude.asm'"
			"${INPUT}"
	WORKING_DIRECTORY "${WORKDIR}"
	RESULT_VARIABLE rc
	OUTPUT_VARIABLE out_text
	ERROR_VARIABLE err_text
)

get_filename_component(exp_ext "${EXPECTED}" EXT)

string(REPLACE "\r\n" "\n" out_text "${out_text}")
string(REPLACE "\r\n" "\n" err_text "${err_text}")

if(exp_ext STREQUAL ".err")
	if(rc EQUAL 0)
		message(FATAL_ERROR "Expected yasme_cli to fail for ${INPUT}, but it succeeded.")
	endif()

	file(READ "${EXPECTED}" exp_text)
	string(REPLACE "\r\n" "\n" exp_text "${exp_text}")

	set(actual "${out_text}\n${err_text}")
	string(STRIP "${actual}" actual)
	string(STRIP "${exp_text}" exp_text)

	if(NOT actual STREQUAL exp_text)
		message(FATAL_ERROR
			"Mismatch for ${INPUT}\n"
			"--- actual ---\n${actual}\n"
			"--- expected ---\n${exp_text}\n"
		)
	endif()

	return()
endif()

if(NOT rc EQUAL 0)
	message(FATAL_ERROR
		"yasme_cli failed for ${INPUT} (exit ${rc})\n"
		"stdout:\n${out_text}\n"
		"stderr:\n${err_text}\n"
	)
endif()

file(READ "${OUTFILE}" out_hex HEX)
string(TOLOWER "${out_hex}" out_hex)

set(exp_hex "")

if(exp_ext STREQUAL ".bin")
	file(READ "${EXPECTED}" exp_hex HEX)
	string(TOLOWER "${exp_hex}" exp_hex)

elseif(exp_ext STREQUAL ".hex")
	file(READ "${EXPECTED}" exp_text)
	string(REGEX MATCHALL "(0x)?[0-9A-Fa-f]{2}" tokens "${exp_text}")
	foreach(t IN LISTS tokens)
		string(REPLACE "0x" "" t "${t}")
		string(TOLOWER "${t}" t)
		string(APPEND exp_hex "${t}")
	endforeach()

elseif(exp_ext STREQUAL ".bytes")
	file(READ "${EXPECTED}" exp_text)
	string(REPLACE "\r\n" "\n" exp_text "${exp_text}")
	string(REPLACE "\r" "\n" exp_text "${exp_text}")

	set(exp_hex "")
	set(_hex_digits 0 1 2 3 4 5 6 7 8 9 a b c d e f)

	string(REPLACE "\n" ";" _lines "${exp_text}")

	foreach(line ${_lines})
		string(REGEX REPLACE ";.*$" "" line "${line}")
		string(REGEX REPLACE "#.*$" "" line "${line}")
		string(STRIP "${line}" line)
		if(line STREQUAL "")
			continue()
		endif()

		string(REPLACE "," " " line "${line}")
		string(REPLACE "\t" " " line "${line}")
		separate_arguments(_toks UNIX_COMMAND "${line}")

		foreach(tok IN LISTS _toks)
			string(STRIP "${tok}" tok)
			if(tok STREQUAL "")
				continue()
			endif()

			string(TOLOWER "${tok}" t)

			if(t MATCHES "^0x[0-9a-f]{1,2}$")
				string(REGEX REPLACE "^0x" "" t "${t}")
			endif()

			if(t MATCHES "^[0-9a-f]$")
				string(APPEND exp_hex "0${t}")
			elseif(t MATCHES "^[0-9a-f][0-9a-f]$")
				string(APPEND exp_hex "${t}")

			elseif(t MATCHES "^[0-9]{1,3}$")
				math(EXPR n "${t}")
				if(n LESS 0 OR n GREATER 255)
					message(FATAL_ERROR "Byte out of range in ${EXPECTED}: ${n}")
				endif()

				math(EXPR _hi "${n} / 16")
				math(EXPR _lo "${n} % 16")
				list(GET _hex_digits ${_hi} _hi_ch)
				list(GET _hex_digits ${_lo} _lo_ch)
				string(APPEND exp_hex "${_hi_ch}${_lo_ch}")
			else()
				message(FATAL_ERROR "Invalid byte token in ${EXPECTED}: '${tok}'")
			endif()
		endforeach()
	endforeach()

else()
	message(FATAL_ERROR "Unsupported expected extension '${exp_ext}' for ${EXPECTED}")
endif()

string(LENGTH "${exp_hex}" _exp_hex_len)
math(EXPR _exp_hex_mod "${_exp_hex_len} % 2")
if(NOT _exp_hex_mod EQUAL 0 OR NOT exp_hex MATCHES "^[0-9a-f]*$")
	file(READ "${EXPECTED}" _exp_preview LIMIT 200)
	message(FATAL_ERROR
		"expected parsing produced invalid exp_hex\n"
		"expected: ${EXPECTED}\n"
		"exp_hex: '${exp_hex}'\n"
		"preview:\n${_exp_preview}\n"
	)
endif()

if(NOT out_hex STREQUAL exp_hex)
	string(LENGTH "${out_hex}" out_hex_len)
	string(LENGTH "${exp_hex}" exp_hex_len)

	set(min_len "${out_hex_len}")
	if(exp_hex_len LESS min_len)
		set(min_len "${exp_hex_len}")
	endif()

	set(first_mismatch -1)
	set(i 0)
	while(i LESS min_len)
		string(SUBSTRING "${out_hex}" ${i} 1 a)
		string(SUBSTRING "${exp_hex}" ${i} 1 b)
		if(NOT a STREQUAL b)
			set(first_mismatch "${i}")
			break()
		endif()
		math(EXPR i "${i} + 1")
	endwhile()

	if(first_mismatch EQUAL -1)
		set(first_mismatch "${min_len}")
	endif()

	math(EXPR byte_off "${first_mismatch} / 2")
	math(EXPR hex_off "${byte_off} * 2")

	set(act_byte "??")
	set(exp_byte "??")

	if(out_hex_len GREATER hex_off)
		if(out_hex_len GREATER_EQUAL "${hex_off} + 2")
			string(SUBSTRING "${out_hex}" ${hex_off} 2 act_byte)
		endif()
	endif()

	if(exp_hex_len GREATER hex_off)
		if(exp_hex_len GREATER_EQUAL "${hex_off} + 2")
			string(SUBSTRING "${exp_hex}" ${hex_off} 2 exp_byte)
		endif()
	endif()

	if(NOT act_byte MATCHES "^[0-9a-f][0-9a-f]$")
		set(act_byte "??")
	endif()
	if(NOT exp_byte MATCHES "^[0-9a-f][0-9a-f]$")
		set(exp_byte "??")
	endif()

	math(EXPR out_bytes "${out_hex_len} / 2")
	math(EXPR exp_bytes "${exp_hex_len} / 2")

	message(FATAL_ERROR
		"Byte mismatch for ${INPUT}\n"
		"\n"
		"produced:\n  ${OUTFILE}\n"
		"\n"
		"expected:\n  ${EXPECTED}\n"
		"\n"
		"first mismatch at byte offset ${byte_off}:\n"
		"\n"
		"  actual:   0x${act_byte}\n"
		"  expected: 0x${exp_byte}\n"
		"\n"
		"lengths: actual=${out_bytes} expected=${exp_bytes}\n"
	)
endif()
