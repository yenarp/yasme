#ifndef YASME_MACRO_TOKEN_SLICE_HH
#define YASME_MACRO_TOKEN_SLICE_HH

#include <string>
#include <yasme/lex/Tokens.hh>
#include <yasme/support/Span.hh>

namespace yasme::macro
{
	struct TokenSlice
	{
		lex::Token const* begin{};
		lex::Token const* end{};
		SourceSpan span{};

		[[nodiscard]] bool empty() const noexcept { return begin == end; }
	};

	[[nodiscard]] SourceSpan span_for_tokens(lex::Token const* begin,
											 lex::Token const* end) noexcept;
	[[nodiscard]] TokenSlice make_token_slice(lex::Token const* begin,
											  lex::Token const* end) noexcept;
	[[nodiscard]] std::string token_slice_to_string(TokenSlice slice);

} // namespace yasme::macro

#endif /* YASME_MACRO_TOKEN_SLICE_HH */
