
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

#ifndef ABYSS_PARSE_NODES_HPP
#define ABYSS_PARSE_NODES_HPP
#pragma once

#include <cstdint>

namespace parser {

using type = std::uint8_t;

#define ABYSS_PARSE_NODES(X) \
	X(cons)                  \
	X(atom)                  \
	X(null)                  \
	X(eoi)                   \
	X(expect_atom)           \
	X(expect_rparen)         \
	X(unexpected_eoi)        \
	X(unexpected_dot)        \
	X(unexpected_initial)

#define ABYSS_PARSE_NODES_ENUM(name) name,

enum class node : type
{
	ABYSS_PARSE_NODES(ABYSS_PARSE_NODES_ENUM)
};

#undef ABYSS_PARSE_NODES_ENUM

}

#endif
