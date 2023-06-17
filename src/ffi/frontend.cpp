
// ISC License (ISC)
//
// Copyright 2023 James Adam Armstrong
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
// REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
// AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
// INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
// LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
// OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
// PERFORMANCE OF THIS SOFTWARE.

#include <abyss/lex/tokenize.hpp>
#include <abyss/parse/parse.hpp>
#include <cassert>
#include <cstdio>
#include <cstdint>
#include <algorithm>
#include <filesystem>
#include <limits>
#include <memory>
#include <string_view>

#ifndef ABYSS_EXPORT
#if defined(_MSC_VER) && defined(_DLL)
#define ABYSS_EXPORT __declspec(dllexport)
#else
#define ABYSS_EXPORT
#endif
#endif

namespace fs = std::filesystem;

struct failure
{
	uint32_t offset;
	uint32_t length;
	uint32_t line;
	uint32_t unmatched;
	token::id id; // set to eoi if representing a parser error
	parser::node node;
};

struct slice
{
	uint32_t offset;
	uint32_t length;
};

struct success
{
	parser::node const *ast;
	token::id const *atoms;
	slice const *slices;
};

struct loaded
{
	char8_t const *data; // nullptr if invalid
	uint32_t size;		 // unless size is zero, which means empty file
};

inline slice make_slice(
	char8_t const *base, std::span<char8_t const> src) noexcept
{
	return slice{static_cast<uint32_t>(src.data() - base),
		static_cast<uint32_t>(src.size())};
}

template<typename T>
inline uint32_t calculate_line(std::vector<T> const &lines, T offset) noexcept
{
	return static_cast<uint32_t>(
		(std::lower_bound(lines.begin(), lines.end(), offset) - lines.begin()) +
		1);
}

inline failure lex_failure(char8_t const *base, std::span<char8_t const> src,
	uint32_t line, token::id err) noexcept
{
	auto [offset, length] = make_slice(base, src);
	return failure{offset, length, line, 0, err, parser::node::eoi};
}

inline failure parse_failure(char8_t const *base, std::span<char8_t const> src,
	uint32_t line, uint32_t unmatched, parser::node err) noexcept
{
	auto [offset, length] = make_slice(base, src);
	return failure{offset, length, line, unmatched, token::id::eoi, err};
}

inline constexpr success wipe_success() noexcept
{
	return success{nullptr, nullptr, nullptr};
}

extern "C" {

loaded ABYSS_EXPORT abyss_load_file(
	char8_t const *name, uint32_t length, failure *f, success *s) noexcept
{
	using std::unique_ptr;
	using std::make_unique;
	assert(f);
	assert(s);
	FILE *file = std::fopen(reinterpret_cast<char const *>(name), "rb");
	if (!file) {
		return loaded{nullptr, 1};
	}
	fs::path file_path{std::u8string_view{name, length}};
	auto const fsize = fs::file_size(file_path);
	if (fsize < 1) {
		std::fclose(file);
		return loaded{nullptr, 0};
	} else if (fsize >= std::numeric_limits<uint32_t>::max()) {
		std::fclose(file);
		return loaded{nullptr, std::numeric_limits<uint32_t>::max()};
	}
	uint32_t const size = fsize + 1;
	unique_ptr<char8_t[]> buffer = make_unique<char8_t[]>(size);
	std::fread(buffer.get(), size, 1, file);
	buffer[size - 1] = 0;
	{
		int code = std::ferror(file);
		std::fclose(file);
		if (code != 0) {
			return loaded{nullptr, 2};
		}
	}
	auto [token_id, token_src, lines, good] =
		token::process({buffer.get(), size});
	if (!good) {
		auto &src = token_src.back();
		*f = lex_failure(buffer.get(), src, calculate_line(lines, src.begin()),
			token_id.back());
		*s = wipe_success();
		return loaded{buffer.release(), size};
	}
	auto [nodes, indices, unmatched, cursor] = parser::parse(token_id);
	if (nodes.back() == parser::node::eoi) {
		unique_ptr<parser::node[]> ast =
			make_unique<parser::node[]>(nodes.size());
		unique_ptr<token::id[]> atoms =
			make_unique<token::id[]>(token_id.size() - 1);
		unique_ptr<slice[]> slices = make_unique<slice[]>(token_id.size() - 1);
		auto cur_atom = atoms.get();
		auto cur_slice = slices.get();
		for (std::size_t n = 0; n < nodes.size(); n++) {
			parser::node n_type = nodes[n];
			ast[n] = n_type;
			if (n_type == parser::node::atom) {
				std::size_t i = indices[n];
				token::id t_type = token_id[i];
				*cur_atom = t_type;
				++cur_atom;
				if (t_type >= token::id::symbol) {
					auto const &src = token_src[i];
					*cur_slice = make_slice(buffer.get(), src);
					++cur_slice;
				}
			}
		}
		*s = success{ast.release(), atoms.release(), slices.release()};
	} else {
		auto &src = token_src.back();
		*f = parse_failure(buffer.get(), src,
			calculate_line(lines, src.begin()), unmatched, parser::node::eoi);
		*s = wipe_success();
	}
	return loaded{buffer.release(), size};
}

void ABYSS_EXPORT abyss_unload(loaded *target, success *tree) noexcept
{
	assert(target);
	loaded &unload = *target;
	if (unload.data) {
		delete[] unload.data;
		unload.data = nullptr;
	}
	if (tree) {
		success &s = *tree;
		if (s.ast) {
			delete[] s.ast;
			s.ast = nullptr;
		}
		if (s.atoms) {
			delete[] s.atoms;
			s.atoms = nullptr;
		}
		if (s.slices) {
			delete[] s.slices;
			s.slices = nullptr;
		}
	}
}
}
