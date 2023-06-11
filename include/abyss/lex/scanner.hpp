
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

#ifndef ABYSS_LEX_SCANNER_HPP
#define ABYSS_LEX_SCANNER_HPP
#pragma once

#include <abyss/lex/tokens.hpp>
#include <cassert>
#include <cstdlib>
#include <span>
#include <tuple>

class scanner
{
public:
	using buffer_t = std::span<char8_t const>;
	using pos_t = buffer_t::iterator;

	constexpr scanner(buffer_t source) noexcept : input(source), cursor(input.begin()) {}
	~scanner() noexcept = default;

	std::tuple<buffer_t, token::id> next() noexcept
	{
		assert(!done());
		pos_t start{cursor};
		token::id id{next_impl()};
		std::size_t length = static_cast<std::size_t>(cursor - start);
		return {buffer_t{start, length}, id};
	}

	bool done() const noexcept { return cursor >= input.end(); }

private:
	buffer_t const input;
	pos_t cursor;

	token::id next_impl() noexcept;

	char8_t peek() const noexcept { return *cursor; }

	bool remaining(std::size_t count) const noexcept { return (cursor + count) >= input.end(); }
};

#endif
