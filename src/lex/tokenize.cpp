
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

#include <abyss/lex/tokenize.hpp>
#include <algorithm>
#include <array>
#include <cassert>
#include <span>
#include <tuple>
#include <vector>

namespace token {

results process(std::span<char8_t const> input) noexcept
{
	scanner scan{input};
	std::vector<id> id_out;
	std::vector<scanner::buffer_t> src_out;
	std::vector<scanner::pos_t> line_out;
	while (true) {
		auto const [src, found] = scan.next();
		switch (flags(found)) {
		case match::atom:
			id_out.push_back(found);
			src_out.push_back(src);
			continue;
		case match::punct:
			id_out.push_back(found);
			src_out.push_back(src);
			if (found != id::eoi) {
				continue;
			} else {
				line_out.push_back(src.begin());
				return results{std::move(id_out), std::move(src_out),
					std::move(line_out), true};
			}
		case match::omit:
			if (found == id::newline) {
				line_out.push_back(src.begin());
			}
			continue;
		case match::invalid:
			id_out.push_back(found);
			src_out.push_back(src);
			return results{std::move(id_out), std::move(src_out),
				std::move(line_out), false};
		}
	}
}

}
