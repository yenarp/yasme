add_executable(yasme_test_runner
	TestRunner.cc
)

target_compile_features(yasme_test_runner PRIVATE cxx_std_23)

if(MSVC)
	target_compile_options(yasme_test_runner PRIVATE /W4 /permissive-)
else()
	target_compile_options(yasme_test_runner PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()

if(TARGET yasme)
	target_link_libraries(yasme_test_runner PRIVATE yasme)
elseif(TARGET yasme_static)
	target_link_libraries(yasme_test_runner PRIVATE yasme_static)
else()
	target_sources(yasme_test_runner PRIVATE $<TARGET_OBJECTS:yasme_obj>)
	target_include_directories(yasme_test_runner PRIVATE
		$<TARGET_PROPERTY:yasme_obj,INTERFACE_INCLUDE_DIRECTORIES>
	)
endif()

set(YASME_TEST_CASES_DIR "${CMAKE_CURRENT_SOURCE_DIR}/cases")
set(YASME_TEST_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/include")

file(GLOB YASME_TEST_ASM_FILES CONFIGURE_DEPENDS
	"${YASME_TEST_CASES_DIR}/*.asm"
)

foreach(asm_file IN LISTS YASME_TEST_ASM_FILES)
	get_filename_component(stem "${asm_file}" NAME_WE)

	set(exp_bytes "${YASME_TEST_CASES_DIR}/${stem}.bytes")
	set(exp_hex   "${YASME_TEST_CASES_DIR}/${stem}.hex")
	set(exp_bin   "${YASME_TEST_CASES_DIR}/${stem}.bin")
	set(exp_err   "${YASME_TEST_CASES_DIR}/${stem}.err")

	set(expected "")
	if(EXISTS "${exp_err}")
		set(expected "${exp_err}")
	elseif(EXISTS "${exp_bytes}")
		set(expected "${exp_bytes}")
	elseif(EXISTS "${exp_hex}")
		set(expected "${exp_hex}")
	elseif(EXISTS "${exp_bin}")
		set(expected "${exp_bin}")
	else()
		message(FATAL_ERROR
			"Missing expected output for ${asm_file}. "
			"Create ${stem}.bytes, ${stem}.hex, or ${stem}.bin in ${YASME_TEST_CASES_DIR}."
		)
	endif()

	add_test(
		NAME "yasme.${stem}"
		COMMAND $<TARGET_FILE:yasme_test_runner> "${asm_file}" "${expected}" -I "${YASME_TEST_INCLUDE_DIR}"
	)

	set_tests_properties("yasme.${stem}" PROPERTIES
		WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
	)
endforeach()

add_custom_target(check
	COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure
	DEPENDS yasme_test_runner yasme_pattern_tests
)

add_executable(yasme_pattern_tests
	PatternTests.cc
)

target_compile_features(yasme_pattern_tests PRIVATE cxx_std_23)

if(MSVC)
	target_compile_options(yasme_pattern_tests PRIVATE /W4 /permissive-)
else()
	target_compile_options(yasme_pattern_tests PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()

if(TARGET yasme)
	target_link_libraries(yasme_pattern_tests PRIVATE yasme)
elseif(TARGET yasme_static)
	target_link_libraries(yasme_pattern_tests PRIVATE yasme_static)
else()
	target_sources(yasme_pattern_tests PRIVATE $<TARGET_OBJECTS:yasme_obj>)
	target_include_directories(yasme_pattern_tests PRIVATE
		$<TARGET_PROPERTY:yasme_obj,INTERFACE_INCLUDE_DIRECTORIES>
	)
endif()

add_test(
	NAME "yasme.patterns"
	COMMAND $<TARGET_FILE:yasme_pattern_tests>
)
