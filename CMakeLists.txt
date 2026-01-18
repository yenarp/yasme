cmake_minimum_required(VERSION 3.24)

project(YeintsAssemblerEngine
	VERSION 0.1.0
	LANGUAGES C CXX ASM
)

option(YASME_BUILD_STATIC "Build static library" ON)
option(YASME_BUILD_SHARED "Build shared library" ON)
option(YASME_BUILD_EXE "Build executable" ON)
option(YASME_ENABLE_LTO "Enable LTO/IPO" ON)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

if(NOT CMAKE_CONFIGURATION_TYPES AND NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

file(GLOB_RECURSE YASME_PUBLIC_HEADERS CONFIGURE_DEPENDS
	"${CMAKE_CURRENT_SOURCE_DIR}/include/*.hh"
	"${CMAKE_CURRENT_SOURCE_DIR}/include/*.tcc"
)

file(GLOB_RECURSE YASME_SOURCES CONFIGURE_DEPENDS
	"${CMAKE_CURRENT_SOURCE_DIR}/source/*.cc"
)

file(GLOB_RECURSE YASME_DRIVER_SOURCES CONFIGURE_DEPENDS
	"${CMAKE_CURRENT_SOURCE_DIR}/driver/*.cc"
)

add_library(yasme_obj OBJECT
	${YASME_PUBLIC_HEADERS}
	${YASME_SOURCES}
)

set_target_properties(yasme_obj PROPERTIES
	POSITION_INDEPENDENT_CODE ON
)

target_include_directories(yasme_obj
	PUBLIC
		$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
		$<INSTALL_INTERFACE:include>
)

target_compile_features(yasme_obj PUBLIC cxx_std_23)

if(MSVC)
	target_compile_options(yasme_obj PRIVATE /W4 /permissive-)
else()
	target_compile_options(yasme_obj PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()

if(YASME_BUILD_STATIC)
	add_library(yasme_static STATIC)
	target_sources(yasme_static PRIVATE $<TARGET_OBJECTS:yasme_obj>)
	target_link_libraries(yasme_static PUBLIC)

	target_include_directories(yasme_static PUBLIC
		$<TARGET_PROPERTY:yasme_obj,INTERFACE_INCLUDE_DIRECTORIES>
	)

	target_compile_features(yasme_static PUBLIC cxx_std_23)

	set_target_properties(yasme_static PROPERTIES
		OUTPUT_NAME yasme
	)
endif()

if(YASME_BUILD_SHARED)
	add_library(yasme SHARED)
	target_sources(yasme PRIVATE $<TARGET_OBJECTS:yasme_obj>)
	target_link_libraries(yasme PUBLIC)

	target_include_directories(yasme PUBLIC
		$<TARGET_PROPERTY:yasme_obj,INTERFACE_INCLUDE_DIRECTORIES>
	)

	target_compile_features(yasme PUBLIC cxx_std_23)

	if(WIN32)
		set_target_properties(yasme PROPERTIES
			WINDOWS_EXPORT_ALL_SYMBOLS ON
		)
	endif()
endif()

if(YASME_BUILD_EXE)
	add_executable(yasme_cli
		${YASME_DRIVER_SOURCES}
	)

	if(TARGET yasme)
		target_link_libraries(yasme_cli PRIVATE yasme)
	elseif(TARGET yasme_static)
		target_link_libraries(yasme_cli PRIVATE yasme_static)
	else()
		target_sources(yasme_cli PRIVATE $<TARGET_OBJECTS:yasme_obj>)
		target_include_directories(yasme_cli PRIVATE
			$<TARGET_PROPERTY:yasme_obj,INTERFACE_INCLUDE_DIRECTORIES>
		)
	endif()

	target_compile_features(yasme_cli PRIVATE cxx_std_23)
endif()

if(YASME_ENABLE_LTO)
	include(CheckIPOSupported)

	check_ipo_supported(
		RESULT YASME_IPO_OK
		OUTPUT YASME_IPO_ERR
		LANGUAGES CXX
	)

	if(YASME_IPO_OK)
		foreach(t yasme_obj yasme yasme_static yasme_cli)
			if(TARGET ${t})
				set_property(TARGET ${t} PROPERTY INTERPROCEDURAL_OPTIMIZATION_RELEASE TRUE)
				set_property(TARGET ${t} PROPERTY INTERPROCEDURAL_OPTIMIZATION_RELWITHDEBINFO TRUE)
			endif()
		endforeach()
	else()
		message(STATUS "LTO/IPO disabled: ${YASME_IPO_ERR}")
	endif()
endif()

include(GNUInstallDirs)

install(DIRECTORY include/ DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

if(TARGET yasme_static)
	install(TARGETS yasme_static
		ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
	)
endif()

if(TARGET yasme)
	install(TARGETS yasme
		RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
		LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
		ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
	)
endif()

if(TARGET yasme_cli)
	install(TARGETS yasme_cli
		RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
	)
endif()

include(CTest)
if(BUILD_TESTING)
	add_subdirectory(tests)
endif()
