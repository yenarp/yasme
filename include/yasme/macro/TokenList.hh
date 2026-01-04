#ifndef YASME_MACRO_TOKEN_LIST_HH
#define YASME_MACRO_TOKEN_LIST_HH

#include <vector>
#include <yasme/lex/Tokens.hh>
#include <yasme/macro/TokenSlice.hh>

namespace yasme::macro
{
	struct TokenList
	{
		std::vector<lex::Token> tokens{};

		[[nodiscard]] TokenSlice slice() const noexcept
		{
			if (tokens.empty())
				return {};
			return make_token_slice(tokens.data(), tokens.data() + tokens.size());
		}
	};

} // namespace yasme::macro

#endif /* YASME_MACRO_TOKEN_LIST_HH */
