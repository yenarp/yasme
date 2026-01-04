#ifndef YASME_MACRO_TOKEN_BUFFER_HH
#define YASME_MACRO_TOKEN_BUFFER_HH

#include <span>
#include <vector>
#include <yasme/lex/Lexer.hh>

namespace yasme::macro
{
	class TokenBuffer
	{
	public:
		TokenBuffer() = default;
		TokenBuffer(SourceManager const& sources, FileId file, lex::LexerOptions opt = {});

		void reset(SourceManager const& sources, FileId file, lex::LexerOptions opt = {});

		[[nodiscard]] std::span<const lex::Token> tokens() const noexcept
		{
			return {m_tokens.data(), m_tokens.size()};
		}

		[[nodiscard]] std::span<const lex::LexError> errors() const noexcept
		{
			return {m_errors.data(), m_errors.size()};
		}

		[[nodiscard]] bool has_error() const noexcept { return !m_errors.empty(); }

	private:
		std::vector<lex::Token> m_tokens{};
		std::vector<lex::LexError> m_errors{};
	};

} // namespace yasme::macro

#endif /* YASME_MACRO_TOKEN_BUFFER_HH */
