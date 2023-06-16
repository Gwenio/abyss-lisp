
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

#ifndef ABYSS_LEX_TOKENS_HPP
#define ABYSS_LEX_TOKENS_HPP
#pragma once

#include <cstdint>

namespace token {

using type = std::uint8_t;

enum class match : type
{
	atom = 0b00000000,
	punct = 0b01000000,
	omit = 0b10000000,
	invalid = 0b11000000,
};

#define ABYSS_LEX_TOKENS_ATOM(X) \
	X(null, atom)                \
	X(inert, atom)               \
	X(ignore, atom)              \
	X(true_, atom)               \
	X(false_, atom)              \
	X(symbol, atom)              \
	X(string, atom)              \
	X(int_dec, atom)             \
	X(int_bin, atom)             \
	X(int_oct, atom)             \
	X(int_hex, atom)

#define ABYSS_LEX_TOKENS_PUNCT(X) \
	X(lparen, punct)              \
	X(rparen, punct)              \
	X(dot, punct)                 \
	X(eoi, punct)

#define ABYSS_LEX_TOKENS_OMIT(X) \
	X(newline, omit)             \
	X(whitespace, omit)          \
	X(line_comment, omit)        \
	X(inline_comment, omit)

#define ABYSS_LEX_TOKENS_INVALID(X)    \
	X(reserved, invalid)               \
	X(invalid_input, invalid)          \
	X(invalid_cr, invalid)             \
	X(invalid_null, invalid)           \
	X(invalid_inline_comment, invalid) \
	X(invalid_float_type, invalid)     \
	X(invalid_int_type, invalid)       \
	X(invalid_string, invalid)         \
	X(invalid_cp, invalid)             \
	X(invalid_cp_esc, invalid)         \
	X(invalid_cp_hex, invalid)         \
	X(invalid_cp_hex9, invalid)        \
	X(invalid_cp_hex0, invalid)        \
	X(invalid_ignore, invalid)         \
	X(invalid_trailing_dot, invalid)

#define ABYSS_LEX_TOKENS(X)   \
	ABYSS_LEX_TOKENS_ATOM(X)  \
	ABYSS_LEX_TOKENS_PUNCT(X) \
	ABYSS_LEX_TOKENS_OMIT(X)  \
	ABYSS_LEX_TOKENS_INVALID(X)

#define ABYSS_LEX_TOKENS_RAW_ENUM(name, _unused) name,

enum class raw_atom : type
{
	ABYSS_LEX_TOKENS_ATOM(ABYSS_LEX_TOKENS_RAW_ENUM)
	//
	raw_max,
};

static_assert(static_cast<type>(raw_atom::raw_max) <= static_cast<type>(match::punct));

enum class raw_punct : type
{
	ABYSS_LEX_TOKENS_PUNCT(ABYSS_LEX_TOKENS_RAW_ENUM)
	//
	raw_max,
};

static_assert(static_cast<type>(raw_punct::raw_max) <= static_cast<type>(match::punct));

enum class raw_omit : type
{
	ABYSS_LEX_TOKENS_OMIT(ABYSS_LEX_TOKENS_RAW_ENUM)
	//
	raw_max,
};

static_assert(static_cast<type>(raw_omit::raw_max) <= static_cast<type>(match::punct));

enum class raw_invalid : type
{
	ABYSS_LEX_TOKENS_INVALID(ABYSS_LEX_TOKENS_RAW_ENUM)
	//
	raw_max,
};

static_assert(static_cast<type>(raw_invalid::raw_max) <= static_cast<type>(match::punct));

#undef ABYSS_LEX_TOKENS_RAW_ENUM

inline constexpr type operator|(raw_atom x, match y) noexcept
{
	return static_cast<type>(x) | static_cast<type>(y);
}

inline constexpr type operator|(raw_punct x, match y) noexcept
{
	return static_cast<type>(x) | static_cast<type>(y);
}

inline constexpr type operator|(raw_omit x, match y) noexcept
{
	return static_cast<type>(x) | static_cast<type>(y);
}

inline constexpr type operator|(raw_invalid x, match y) noexcept
{
	return static_cast<type>(x) | static_cast<type>(y);
}

#define ABYSS_LEX_TOKENS_ENUM(name, flags) name = raw_##flags::name | match::flags,

enum class id : type
{
	ABYSS_LEX_TOKENS(ABYSS_LEX_TOKENS_ENUM)
};

#undef ABYSS_LEX_TOKENS_ENUM

inline constexpr match flags(id x) noexcept
{
	return static_cast<match>(static_cast<type>(x) & static_cast<type>(match::invalid));
}

}

#endif
