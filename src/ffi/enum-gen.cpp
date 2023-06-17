
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
#include <string>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <vector>
#include <type_traits>

namespace fs = std::filesystem;
using std::cout;
using std::endl;

std::string process_name(std::string_view name)
{
	if (*name.rbegin() == '_') {
		name = name.substr(0, name.size() - 1);
	}
	std::string temp{name};
	for (auto x = temp.find('_', 0); x != std::string::npos;
		 x = temp.find('_', x)) {
		temp[x] = '-';
	}
	return std::move(temp);
}

int main()
{
	std::string_view const filename{"src/ffi/enums.lisp"};
	fs::path file_path{filename};
	std::ofstream file{file_path, std::ios::out | std::ios::trunc};
	if (!file.is_open()) {
		cout << "Failed to open file: " << filename << endl;
		if (!fs::is_regular_file(file_path)) {
			cout << "The target exists and was not a file: " << filename
				 << endl;
			return 1;
		}
		return 1;
	}
	file << "; generated file" << endl;
	file << "(uiop:define-package :abyss/ffi/enums" << endl;
	file << "\t(:use :cl)" << endl;
	file << "\t(:import-from :cffi :defcenum)" << endl;
	file << "\t(:export :tokens :nodes)" << endl << ")" << endl;
	file << "(cl:in-package :abyss/ffi/enums)" << endl << endl;
	static_assert(std::is_same_v<token::type, std::uint8_t>);
	file << "(defcenum (tokens :uint8)" << endl;

#define PRINT_TOKEN(type, _unused)               \
	file << "\t(:" << process_name(#type) << " " \
		 << static_cast<std::size_t>(token::id::type) << ")" << endl;

	ABYSS_LEX_TOKENS(PRINT_TOKEN);

	file << ")" << endl << endl;
	static_assert(std::is_same_v<parser::type, std::uint8_t>);
	file << "(defcenum (nodes :uint8)" << endl;

#define PRINT_NODE(type)                         \
	file << "\t(:" << process_name(#type) << " " \
		 << static_cast<std::size_t>(parser::node::type) << ")" << endl;

	ABYSS_PARSE_NODES(PRINT_NODE);

	file << ")" << endl;
	return 0;
}
