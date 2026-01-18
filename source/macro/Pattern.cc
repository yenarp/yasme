#include <cstddef>
#include <utility>
#include <yasme/macro/Pattern.hh>

namespace yasme::macro
{
	static bool is_literal_kind(lex::TokenKind kind) noexcept
	{
		return kind == lex::TokenKind::identifier || kind == lex::TokenKind::integer
			   || kind == lex::TokenKind::string || kind == lex::TokenKind::char_literal;
	}

	static bool tokens_equivalent(lex::Token const& a, lex::Token const& b) noexcept
	{
		if (a.kind != b.kind)
			return false;

		if (is_literal_kind(a.kind))
			return a.lexeme == b.lexeme;

		return true;
	}

	static bool slice_equivalent(TokenSlice a, TokenSlice b) noexcept
	{
		auto a_len = static_cast<std::size_t>(a.end - a.begin);
		auto b_len = static_cast<std::size_t>(b.end - b.begin);
		if (a_len != b_len)
			return false;

		for (std::size_t i = 0; i < a_len; ++i)
			if (!tokens_equivalent(a.begin[i], b.begin[i]))
				return false;

		return true;
	}

	static PatternLiteral make_literal(lex::Token const& tok)
	{
		PatternLiteral lit{};
		lit.span = tok.span;
		lit.kind = tok.kind;
		if (is_literal_kind(tok.kind))
			lit.lexeme = std::string(tok.lexeme);
		return lit;
	}

	PatternParseResult parse_pattern(TokenSlice slice)
	{
		PatternParseResult out{};

		if (!slice.begin || !slice.end)
			return out;

		auto* begin = slice.begin;
		auto* end = slice.end;
		auto count = static_cast<std::size_t>(end - begin);

		for (std::size_t i = 0; i < count; ++i)
		{
			auto const& tok = begin[i];

			if (tok.kind == lex::TokenKind::lbrace)
			{
				if (i + 1 >= count)
				{
					out.errors.push_back(
						PatternParseError{tok.span, "expected identifier after '{'"});
					break;
				}

				auto const& name_tok = begin[i + 1];
				if (name_tok.kind != lex::TokenKind::identifier)
				{
					out.errors.push_back(
						PatternParseError{name_tok.span, "expected identifier after '{'"});
					++i;
					continue;
				}

				std::size_t close_idx = i + 2;
				if (close_idx >= count || begin[close_idx].kind != lex::TokenKind::rbrace)
				{
					out.errors.push_back(
						PatternParseError{name_tok.span, "expected '}' after binding name"});
					for (; close_idx < count; ++close_idx)
						if (begin[close_idx].kind == lex::TokenKind::rbrace)
							break;
				}

				if (close_idx >= count || begin[close_idx].kind != lex::TokenKind::rbrace)
				{
					i = count;
					break;
				}

				PatternBind bind{};
				bind.span = SourceSpan{name_tok.span.id, tok.span.begin, begin[close_idx].span.end};
				bind.name = std::string(name_tok.lexeme);
				out.pattern.elems.push_back(std::move(bind));
				i = close_idx;
				continue;
			}

			if (tok.kind == lex::TokenKind::rbrace)
			{
				out.errors.push_back(PatternParseError{tok.span, "unexpected '}' in pattern"});
				continue;
			}

			if (tok.kind == lex::TokenKind::ellipsis)
			{
				PatternEllipsis ell{};
				ell.span = tok.span;
				out.pattern.elems.push_back(std::move(ell));
				continue;
			}

			if (tok.kind == lex::TokenKind::identifier && tok.lexeme == "_")
			{
				PatternWildcard wc{};
				wc.span = tok.span;
				out.pattern.elems.push_back(std::move(wc));
				continue;
			}

			out.pattern.elems.push_back(make_literal(tok));
		}

		return out;
	}

	static bool match_from(Pattern const& pattern,
						   TokenSlice input,
						   std::size_t pat_idx,
						   std::size_t tok_idx,
						   MatchResult& result)
	{
		auto pat_size = pattern.elems.size();
		auto tok_size = static_cast<std::size_t>(input.end - input.begin);

		if (pat_idx == pat_size)
			return tok_idx == tok_size;

		auto const& elem = pattern.elems[pat_idx];

		if (auto lit = std::get_if<PatternLiteral>(&elem))
		{
			if (tok_idx >= tok_size)
				return false;

			auto const& tok = input.begin[tok_idx];
			if (tok.kind != lit->kind)
				return false;

			if (is_literal_kind(lit->kind) && tok.lexeme != lit->lexeme)
				return false;

			return match_from(pattern, input, pat_idx + 1, tok_idx + 1, result);
		}

		if (std::holds_alternative<PatternWildcard>(elem))
		{
			if (tok_idx >= tok_size)
				return false;

			return match_from(pattern, input, pat_idx + 1, tok_idx + 1, result);
		}

		if (std::holds_alternative<PatternEllipsis>(elem))
		{
			for (std::size_t len = 0; tok_idx + len <= tok_size; ++len)
				if (match_from(pattern, input, pat_idx + 1, tok_idx + len, result))
					return true;
			return false;
		}

		auto const& bind = std::get<PatternBind>(elem);
		auto max_len = tok_size - tok_idx;
		if (max_len == 0)
			return false;

		for (std::size_t len = 1; len <= max_len; ++len)
		{
			auto slice = make_token_slice(input.begin + tok_idx, input.begin + tok_idx + len);

			auto existing = result.bindings.find(bind.name);
			if (existing != result.bindings.end())
			{
				if (!slice_equivalent(existing->second, slice))
					continue;

				if (match_from(pattern, input, pat_idx + 1, tok_idx + len, result))
					return true;
				continue;
			}

			result.bindings.emplace(bind.name, slice);
			if (match_from(pattern, input, pat_idx + 1, tok_idx + len, result))
				return true;

			result.bindings.erase(bind.name);
		}

		return false;
	}

	bool match_pattern(Pattern const& pattern, TokenSlice input, MatchResult& result)
	{
		result.bindings.clear();
		if (!input.begin || !input.end)
			return pattern.elems.empty();

		return match_from(pattern, input, 0, 0, result);
	}

} // namespace yasme::macro
