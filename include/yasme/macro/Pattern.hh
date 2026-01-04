#ifndef YASME_MACRO_PATTERN_HH
#define YASME_MACRO_PATTERN_HH

#include <string>
#include <unordered_map>
#include <variant>
#include <vector>
#include <yasme/lex/Tokens.hh>
#include <yasme/macro/TokenSlice.hh>
#include <yasme/support/Span.hh>

namespace yasme::macro
{
	struct PatternLiteral
	{
		SourceSpan span{};
		lex::TokenKind kind{lex::TokenKind::invalid};
		std::string lexeme{};
	};

	struct PatternWildcard
	{
		SourceSpan span{};
	};

	struct PatternEllipsis
	{
		SourceSpan span{};
	};

	struct PatternBind
	{
		SourceSpan span{};
		std::string name{};
	};

	using PatternElem = std::variant<PatternLiteral, PatternWildcard, PatternEllipsis, PatternBind>;

	struct Pattern
	{
		std::vector<PatternElem> elems{};
	};

	struct PatternParseError
	{
		SourceSpan span{};
		std::string message{};
	};

	struct PatternParseResult
	{
		Pattern pattern{};
		std::vector<PatternParseError> errors{};

		[[nodiscard]] bool ok() const noexcept { return errors.empty(); }
	};

	PatternParseResult parse_pattern(TokenSlice slice);

	struct MatchResult
	{
		std::unordered_map<std::string, TokenSlice> bindings{};
	};

	[[nodiscard]] bool match_pattern(Pattern const& pattern, TokenSlice input, MatchResult& result);

} // namespace yasme::macro

#endif /* YASME_MACRO_PATTERN_HH */
