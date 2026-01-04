#include <yasme/macro/TokenSlice.hh>

namespace yasme::macro
{
	SourceSpan span_for_tokens(lex::Token const* begin, lex::Token const* end) noexcept
	{
		if (!begin || !end || begin == end)
			return {};

		auto const& first = *begin;
		auto const& last = *(end - 1);

		if (first.span.id == 0)
			return last.span;
		if (last.span.id == 0)
			return first.span;
		if (first.span.id != last.span.id)
			return first.span;

		return SourceSpan{first.span.id, first.span.begin, last.span.end};
	}

	TokenSlice make_token_slice(lex::Token const* begin, lex::Token const* end) noexcept
	{
		TokenSlice slice{};
		slice.begin = begin;
		slice.end = end;
		slice.span = span_for_tokens(begin, end);
		return slice;
	}

	static std::string token_spelling(lex::Token const& tok)
	{
		switch (tok.kind)
		{
			case lex::TokenKind::newline:
				return "\\n";
			case lex::TokenKind::eof:
				return "<eof>";
			case lex::TokenKind::invalid:
				return "<invalid>";
			default:
				break;
		}

		if (!tok.lexeme.empty())
			return std::string(tok.lexeme);

		return std::string(lex::token_kind_name(tok.kind));
	}

	std::string token_slice_to_string(TokenSlice slice)
	{
		if (!slice.begin || !slice.end || slice.begin == slice.end)
			return {};

		std::string out{};
		bool first = true;

		for (auto it = slice.begin; it != slice.end; ++it)
		{
			if (!first)
				out.push_back(' ');

			out += token_spelling(*it);
			first = false;
		}

		return out;
	}

} // namespace yasme::macro
