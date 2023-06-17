
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

#include <abyss/parse/nodes.hpp>
#include <abyss/parse/parse.hpp>
#include <abyss/lex/tokens.hpp>
#include <abyss/lex/tokenize.hpp>
#include <string_view>
#include <filesystem>
#include <iostream>
#include <fstream>
#include <tuple>
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
	std::vector<char8_t> const &buffer, std::span<char8_t const> src,
	std::size_t line)
{
	cout << match_type(x) << " : " << type << " @ "
		 << src.data() - buffer.data() << " + " << src.size() << " on line "
		 << line + 1 << endl;
}

#define PRINT_TOKEN(type, _unused)                                 \
	case id::type:                                                 \
		print_source(id::type, #type, buffer, token_src[x], line); \
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
	auto [token_id, token_src, lines] = token::process(buffer);
	assert(token_id.size() == token_src.size());
	for (std::size_t x = 0; x < token_id.size(); x++) {
		auto line = token::calculate_line(lines, token_src[x].begin());
		switch (token_id[x]) {
			ABYSS_LEX_TOKENS(PRINT_TOKEN);
		}
	}
	if (token_id.back() != id::eoi) {
		cout << "Tokenization failed. Skip parsing." << endl;
		return 1;
	}
	auto [nodes, indices, unmatched, cursor] = parser::parse(token_id);
	cout << endl << "Parse Metrics: " << endl;
	cout << "-\tNodes " << nodes.size() << endl;
	cout << "-\tIndices " << indices.size() << endl;
	cout << "-\tUnmatched '(' " << unmatched << endl;
	cout << "-\tFinal offset " << cursor + 1 << " / " << token_id.size()
		 << endl;
	if (nodes.back() == parser::node::eoi) {
		cout << "-\tParsing succeeded" << endl << endl;
	} else {
		cout << "-\tParsing failed" << endl << endl;
	}

#define PRINT_NODE(type)     \
	case parser::node::type: \
		cout << #type;       \
		break;

	assert(nodes.size() == indices.size());
	for (std::size_t x = 0; x < nodes.size(); x++) {
		switch (nodes[x]) {
			ABYSS_PARSE_NODES(PRINT_NODE);
		}
		cout << " @ " << indices[x] << endl;
	}
	return 0;
}
