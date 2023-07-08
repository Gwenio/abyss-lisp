
project('Abyss Lisp', 'cpp',
	version: '0.2.0',
	license: 'ISC',
	license_files: 'LICENSE',
	default_options: ['cpp_std=c++20']
)

incdir = include_directories('include')

re2c_exe = find_program('re2c')

re2c_cpp = custom_target('re2c_cpp',
	input: 'src/lex/re2c.txt',
	output: 're2c.cpp',
	command: [re2c_exe, '-W', '@INPUT@', '-o', '@OUTPUT@']
)

executable('abyss-lex',
	'src/lex/cli.cpp',
	re2c_cpp,
	include_directories : incdir
)

executable('abyss-parse',
	'src/parse/cli.cpp',
	'src/lex/tokenize.cpp',
	'src/parse/parse.cpp',
	re2c_cpp,
	include_directories : incdir
)

enum_gen = executable('abyss-ffi-enum-gen',
	'src/ffi/enum-gen.cpp',
	include_directories : incdir
)

shared_library('abyss',
	'src/ffi/frontend.cpp',
	'src/lex/tokenize.cpp',
	'src/parse/parse.cpp',
	re2c_cpp,
	include_directories : incdir
)
