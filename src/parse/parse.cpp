
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

#include <abyss/parse/parse.hpp>
#include <cassert>

namespace parser {

// in the following tables:
// rows = states,	columns = input
// 0 = initial,		A = atom (default)
// 1 = head,		L = lparen
// 2 = cons,		R = rparen
// 3 = tail,		D = dot
// 4 = close,		E = end of input
//
// transition notation:
// number = state to transition to
// $ = accept / parsing succeeded
// - = 2 if R inputs, including the latest, less than L input; otherwise 0
// blank = error
//
// transition table
//   A L R D E
// 0 0 1     $
// 1 2 1 -
// 2 2 1 - 3
// 3 4
// 4     -
//
// The only state needed for transitions is a count of unmatched lparens.
// The count is incremented when entering state 1
// And decremented when an rparen is matched before selecting the next state
//
// productions:
// A,0 -> cons, atom
// L,0 -> cons
// E,0 -> eoi
// A,1 -> cons, atom (note: the cons is attributed to previous token)
// L,1 -> cons (note: the cons is attributed to previous token)
// R,1 -> null (note: attributed to previous token)
// A,2 -> cons, atom
// L,2 ->
// R,2 -> null
// D,2 ->
// A,3 -> atom
// R,4 ->
result parse(std::span<token::id> input) noexcept
{
	using token::id;
	std::size_t cursor = 0;
	std::size_t unmatched_lparens = 0;
	// the productions
	std::vector<node> nodes;
	// the associated token for a production
	std::vector<std::size_t> index;
	// there is at most two node per token, end of input only has 1
	// cons cell nodes are often implicit from other nodes
	nodes.reserve(input.size() * 2 - 1);
	// if cons nodes are not given an index, index could be size of input
	index.reserve(input.size() * 2 - 1);
	goto start; // enter initial state, but skip incrementing cursor

#define ABYSS_PARSE_ERROR(type)  \
	nodes.push_back(node::type); \
	index.push_back(cursor);     \
	return result{std::move(nodes), std::move(index), unmatched_lparens, cursor};

initial:
	++cursor;
start:
	switch (input[cursor]) {
	default:
		assert(token::flags(input[cursor]) == token::match::atom);
		nodes.push_back(node::cons);
		nodes.push_back(node::atom);
		index.push_back(cursor);
		index.push_back(cursor);
		goto initial;
	case id::lparen:
		nodes.push_back(node::cons);
		index.push_back(cursor);
		goto head;
	case id::rparen: // error
		ABYSS_PARSE_ERROR(unexpected_initial);
	case id::dot: // error
		ABYSS_PARSE_ERROR(unexpected_initial);
	case id::eoi: // accept
		nodes.push_back(node::eoi);
		index.push_back(cursor);
		return result{std::move(nodes), std::move(index), unmatched_lparens, cursor};
	}
head:
	++cursor;
	++unmatched_lparens;
	switch (input[cursor]) {
	default:
		assert(token::flags(input[cursor]) == token::match::atom);
		nodes.push_back(node::cons);
		nodes.push_back(node::atom);
		index.push_back(cursor - 1);
		index.push_back(cursor);
		goto cons;
	case id::lparen:
		nodes.push_back(node::cons);
		index.push_back(cursor - 1);
		goto head;
	case id::rparen:
		nodes.push_back(node::null);
		index.push_back(cursor - 1);
		if (--unmatched_lparens > 0) {
			goto cons;
		} else {
			goto initial;
		}
	case id::dot: // error
		ABYSS_PARSE_ERROR(unexpected_dot);
	case id::eoi: // error
		ABYSS_PARSE_ERROR(unexpected_eoi);
	}
cons:
	++cursor;
	switch (input[cursor]) {
	default:
		assert(token::flags(input[cursor]) == token::match::atom);
		nodes.push_back(node::cons);
		nodes.push_back(node::atom);
		index.push_back(cursor);
		index.push_back(cursor);
		goto cons;
	case id::lparen:
		nodes.push_back(node::cons);
		index.push_back(cursor);
		goto head;
	case id::rparen:
		nodes.push_back(node::null);
		index.push_back(cursor);
		if (--unmatched_lparens > 0) {
			goto cons;
		} else {
			goto initial;
		}
	case id::dot:
		goto tail;
	case id::eoi: // error
		ABYSS_PARSE_ERROR(unexpected_eoi);
	}
tail:
	++cursor;
	switch (input[cursor]) {
	default:
		assert(token::flags(input[cursor]) == token::match::atom);
		nodes.push_back(node::atom);
		goto close;
	case id::lparen: // error
		ABYSS_PARSE_ERROR(expect_atom);
	case id::rparen: // error
		ABYSS_PARSE_ERROR(expect_atom);
	case id::dot: // error
		ABYSS_PARSE_ERROR(expect_atom);
	case id::eoi: // error
		ABYSS_PARSE_ERROR(expect_atom);
	}
close:
	++cursor;
	switch (input[cursor]) {
	default: // error
		assert(token::flags(input[cursor]) == token::match::atom);
		ABYSS_PARSE_ERROR(expect_rparen);
	case id::lparen: // error
		ABYSS_PARSE_ERROR(expect_rparen);
	case id::rparen:
		if (--unmatched_lparens > 0) {
			goto cons;
		} else {
			goto initial;
		}
	case id::dot: // error
		ABYSS_PARSE_ERROR(expect_rparen);
	case id::eoi: // error
		ABYSS_PARSE_ERROR(expect_rparen);
	}
}

}
