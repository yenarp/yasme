// yasme/lex/lexer.hh
#ifndef YASME_LEX_LEXER_HH
#define YASME_LEX_LEXER_HH

#include <span>
#include <string>
#include <string_view>
#include <vector>
#include <yasme/lex/tokens.hh>
#include <yasme/support/SourceManager.hh>

namespace yasme::lex
{
	struct LexerOptions
	{
		bool emit_newlines{true};
		bool case_insensitive_keywords{true};
		bool enable_c_comments{true};
	};

	struct LexError
	{
		SourceSpan span{};
		std::string message{};
	};

	class Lexer
	{
	public:
		explicit Lexer(SourceManager const& sources, FileId file, LexerOptions opt = {}) noexcept;

		[[nodiscard]] FileId file_id() const noexcept { return m_file; }
		[[nodiscard]] FileByte offset() const noexcept { return m_off; }

		void reset(FileByte offset = 0) noexcept;

		[[nodiscard]] Token peek();
		Token next();

		[[nodiscard]] bool has_error() const noexcept { return !m_errors.empty(); }
		[[nodiscard]] std::span<const LexError> errors() const noexcept
		{
			return {m_errors.data(), m_errors.size()};
		}

	private:
		Token lex_one();

		void skip_spaces_and_comments();

		Token lex_identifier(FileByte begin);
		Token lex_integer(FileByte begin);
		Token lex_string_like(FileByte begin, char quote, TokenKind kind);

		[[nodiscard]] bool at_end() const noexcept;
		[[nodiscard]] char cur() const noexcept;
		[[nodiscard]] char peek_char(FileByte rel = 1) const noexcept;

		void advance(FileByte n = 1) noexcept;

		[[nodiscard]] Token make(TokenKind kind, FileByte begin, FileByte end) const noexcept;

		void add_error(FileByte begin, FileByte end, std::string message);

		SourceManager const* m_sources{nullptr};
		FileId m_file{};
		std::string_view m_input{};

		FileByte m_off{};

		LexerOptions m_opt{};

		bool m_has_peek{};
		Token m_peeked{};

		std::vector<LexError> m_errors{};
	};

} // namespace yasme::lex

#endif /* YASME_LEX_LEXER_HH */
