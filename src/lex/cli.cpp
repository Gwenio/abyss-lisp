
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
#include <abyss/lex/scanner.hpp>
#include <cstdio>
#include <string_view>
#include <filesystem>
#include <iostream>
#include <vector>

namespace fs = std::filesystem;
using token::id;
using std::cout;
using std::endl;

std::string_view match_type(id x) noexcept
{
	using token::match;
	switch (token::flags(x)) {
	case match::atom:
		return {"atom"};
	case match::punct:
		return {"punct"};
	case match::omit:
		return {"omit"};
	case match::invalid:
		return {"invalid"};
	}
	return {"unknown"};
}

void print_source(id x, std::string_view type,
	std::vector<char8_t> const &buffer, std::span<char8_t const> src)
{
	cout << match_type(x) << " : " << type << " @ "
		 << src.data() - buffer.data() << " + " << src.size() << endl;
}

#define PRINT_TOKEN(type, _unused)               \
	case id::type:                               \
		print_source(found, #type, buffer, src); \
		break;

int main(int const argc, char const *const *argv)
{
	if (argc < 2) {
		cout << "Please specify an input file." << endl;
		return -1;
	}
	std::string_view const filename{argv[1]};
	std::vector<char8_t> buffer;
	{
		FILE *source = std::fopen(filename.data(), "r");
		fs::path file_path{filename};
		if (!source) {
			cout << "Failed to open file: " << filename << endl;
			if (!fs::is_regular_file(file_path)) {
				cout << "The target was not a file: " << filename << endl;
				return -1;
			}
			return -1;
		}
		auto const size = fs::file_size(file_path);
		if (size < 1) {
			cout << "The file was empty: " << filename << endl;
			std::fclose(source);
			return 0;
		} else {
			cout << "The file size is: " << size << endl;
		}
		buffer.resize(size + 1);
		std::fread(buffer.data(), size, 1, source);
		std::fclose(source);
	}
	*buffer.rbegin() = '\x00';
	scanner lex{scanner::buffer_t{buffer.cbegin(), buffer.size()}};
	for (std::size_t count = 0; count <= buffer.size(); count++) {
		auto const [src, found] = lex.next();
		switch (found) {
			ABYSS_LEX_TOKENS(PRINT_TOKEN)
		}
		if (found == id::eoi) {
			return 0;
		}
	}
	cout << "Error: got more tokens than input bytes." << endl;
	return 1;
}
