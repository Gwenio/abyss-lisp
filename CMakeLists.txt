cmake_minimum_required(VERSION 3.26)

if(CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR)
	message(FATAL_ERROR "Do not build in-source. Please remove CMakeCache.txt and the CMakeFiles/ directory. Then build out-of-source.")
endif()

project(
	"Abyss Lisp"
	VERSION 0.2.0
	LANGUAGES CXX
)

#set(CMAKE_EXPERIMENTAL_CXX_MODULE_CMAKE_API "2182bf5c-ef0d-489a-91da-49dbc3090d2a")
#set(CMAKE_EXPERIMENTAL_CXX_MODULE_DYNDEP ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
set(CMAKE_EXPORT_COMPILE_COMMANDS True)

include_directories(BEFORE ${CMAKE_CURRENT_SOURCE_DIR}/include/)

if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")

#add_compile_options( /FAu /Fa /utf-8 )
add_compile_options( $<$<CONFIG:Release>:/GL> $<$<CONFIG:Release>:/Gy> )

add_link_options( $<$<CONFIG:Release>:/LTCG> )

endif()

add_custom_command(
	OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/src/lex/re2c.cpp
	COMMAND re2c -W ${CMAKE_CURRENT_SOURCE_DIR}/src/lex/re2c.txt -o ${CMAKE_CURRENT_SOURCE_DIR}/src/lex/re2c.cpp
	MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/src/lex/re2c.txt
)
add_custom_target(gen-re2c
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/src/lex/re2c.cpp
)

add_executable(abyss-lex)
add_dependencies(abyss-lex gen-re2c)
target_sources(abyss-lex PRIVATE
	src/lex/cli.cpp
	src/lex/re2c.cpp
)

add_executable(abyss-parse)
add_dependencies(abyss-parse gen-re2c)
target_sources(abyss-parse PRIVATE
	src/parse/cli.cpp
	src/lex/re2c.cpp
	src/lex/tokenize.cpp
	src/parse/parse.cpp
)

add_executable(abyss-ffi-enum-gen)
target_sources(abyss-ffi-enum-gen PRIVATE
	src/ffi/enum-gen.cpp
)

add_custom_command(
	OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/src/ffi/enums.lisp
	COMMAND abyss-ffi-enum-gen
	DEPENDS abyss-ffi-enum-gen
	WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
add_custom_target(gen-enums
	DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/src/ffi/enums.lisp
)

add_library(abyss SHARED)
add_dependencies(abyss gen-re2c gen-enums)
target_sources(abyss PRIVATE
	src/ffi/frontend.cpp
	src/lex/re2c.cpp
	src/lex/tokenize.cpp
	src/parse/parse.cpp
)
