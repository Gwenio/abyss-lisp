
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

#ifndef ABYSS_PARSE_PARSE_HPP
#define ABYSS_PARSE_PARSE_HPP
#pragma once

#include <abyss/lex/tokens.hpp>
#include <abyss/parse/nodes.hpp>
#include <cstdint>
#include <span>
#include <tuple>
#include <vector>

namespace parser {

using result = std::tuple<std::vector<node>, std::vector<std::size_t>,
	std::size_t, std::size_t>;

result parse(std::span<token::id> input) noexcept;

}

#endif
