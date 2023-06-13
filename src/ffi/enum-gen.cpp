
// ISC License (ISC)
//
// Copyright 2023 James Adam Armstrong
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above copyright
// notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
// REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
// INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
// LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
// OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
// PERFORMANCE OF THIS SOFTWARE.

#include <abyss/lex/tokens.hpp>
#include <abyss/parse/nodes.hpp>
#include <string_view>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <vector>

namespace fs = std::filesystem;
using std::cout;
using std::endl;

int main()
{
	{
		std::string_view const filename{"src/ffi/tokens.lisp"};
		fs::path file_path{filename};
		std::ofstream file{file_path, std::ios::out | std::ios::trunc};
		if (!file.is_open()) {
			cout << "Failed to open file: " << filename << endl;
			if (!fs::is_regular_file(file_path)) {
				cout << "The target exists and was not a file: " << filename << endl;
				return 1;
			}
			return 1;
		}
		file << "; generated file" << endl;
		file << "(uiop:define-package :abyss/ffi/tokens" << endl;
		file << "\t(:use :cl)" << endl;
		file << "\t(:export" << endl;

#define PRINT_TOKEN_EXPORT(type, _unused) file << "\t\t:+" << #type << "+" << endl;

		ABYSS_LEX_TOKENS(PRINT_TOKEN_EXPORT);
		file << "\t)" << endl << ")" << endl;
		file << "(cl:in-package :abyss/ffi/tokens)" << endl;

#define PRINT_TOKEN(type, _unused)                                                                \
	file << "(defconstant +" << #type << "+ " << static_cast<std::size_t>(token::id::type) << ")" \
		 << endl;

		ABYSS_LEX_TOKENS(PRINT_TOKEN);
	}
	{
		std::string_view const filename{"src/ffi/nodes.lisp"};
		fs::path file_path{filename};
		std::ofstream file{file_path, std::ios::out | std::ios::trunc};
		if (!file.is_open()) {
			cout << "Failed to open file: " << filename << endl;
			if (!fs::is_regular_file(file_path)) {
				cout << "The target exists and was not a file: " << filename << endl;
				return 1;
			}
			return 1;
		}
		file << "; generated file" << endl;
		file << "(uiop:define-package :abyss/ffi/nodes" << endl;
		file << "\t(:use :cl)" << endl;
		file << "\t(:export" << endl;

#define PRINT_NODE_EXPORT(type) file << "\t\t:+" << #type << "+" << endl;

		ABYSS_PARSE_NODES(PRINT_NODE_EXPORT);
		file << "\t)" << endl << ")" << endl;
		file << "(cl:in-package :abyss/ffi/nodes)" << endl;

#define PRINT_NODE(type)                                                                      \
	file << "(defconstant +" << #type << "+ " << static_cast<std::size_t>(parser::node::type) \
		 << ")" << endl;

		ABYSS_PARSE_NODES(PRINT_NODE);
	}
	return 0;
}
