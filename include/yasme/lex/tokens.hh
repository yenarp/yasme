#ifndef YASME_LEX_TOKENS_HH
#define YASME_LEX_TOKENS_HH

#include <cstdint>
#include <string_view>
#include <yasme/support/Span.hh>

namespace yasme::lex
{
	enum class TokenKind : std::uint16_t
	{
		invalid,

		eof,
		newline,

		identifier,
		integer,
		string,
		char_literal,

		kw_db,
		kw_dw,
		kw_dd,
		kw_dq,
		kw_org,
		kw_virtual,
		kw_define,
		kw_postpone,
		kw_end,

		lparen,
		rparen,
		lbracket,
		rbracket,
		lbrace,
		rbrace,

		comma,
		colon,
		dot,

		at,
		dollar,
		hash,
		question,

		plus,
		minus,
		star,
		slash,
		percent,

		amp,
		pipe,
		caret,
		tilde,
		bang,

		eq,
		lt,
		gt,

		le,
		ge,
		eqeq,
		ne,

		shl,
		shr,

		andand,
		oror,
	};

	enum class NumberBase : std::uint8_t
	{
		binary = 2,
		octal = 8,
		decimal = 10,
		hexadecimal = 16,
	};

	struct IntegerValue
	{
		std::uint64_t value{};
		NumberBase base{NumberBase::decimal};
		bool overflow{};
	};

	struct Token
	{
		TokenKind kind{TokenKind::invalid};
		SourceSpan span{};

		std::string_view lexeme{};

		IntegerValue integer{};

		[[nodiscard]] constexpr bool is(TokenKind k) const noexcept { return kind == k; }
	};

	[[nodiscard]] constexpr bool is_trivia(TokenKind k) noexcept
	{
		return k == TokenKind::newline;
	}

	[[nodiscard]] constexpr bool is_keyword(TokenKind k) noexcept
	{
		switch (k)
		{
			case TokenKind::kw_db:
			case TokenKind::kw_dw:
			case TokenKind::kw_dd:
			case TokenKind::kw_dq:
			case TokenKind::kw_org:
			case TokenKind::kw_virtual:
			case TokenKind::kw_define:
			case TokenKind::kw_postpone:
			case TokenKind::kw_end:
				return true;
			default:
				return false;
		}
	}

	[[nodiscard]] constexpr bool is_operator(TokenKind k) noexcept
	{
		switch (k)
		{
			case TokenKind::plus:
			case TokenKind::minus:
			case TokenKind::star:
			case TokenKind::slash:
			case TokenKind::percent:
			case TokenKind::amp:
			case TokenKind::pipe:
			case TokenKind::caret:
			case TokenKind::tilde:
			case TokenKind::bang:
			case TokenKind::eq:
			case TokenKind::lt:
			case TokenKind::gt:
			case TokenKind::le:
			case TokenKind::ge:
			case TokenKind::eqeq:
			case TokenKind::ne:
			case TokenKind::shl:
			case TokenKind::shr:
			case TokenKind::andand:
			case TokenKind::oror:
				return true;
			default:
				return false;
		}
	}

	[[nodiscard]] constexpr std::string_view token_kind_name(TokenKind k) noexcept
	{
		switch (k)
		{
			case TokenKind::invalid:
				return "invalid";
			case TokenKind::eof:
				return "eof";
			case TokenKind::newline:
				return "newline";
			case TokenKind::identifier:
				return "identifier";
			case TokenKind::integer:
				return "integer";
			case TokenKind::string:
				return "string";
			case TokenKind::char_literal:
				return "char_literal";

			case TokenKind::kw_db:
				return "kw_db";
			case TokenKind::kw_dw:
				return "kw_dw";
			case TokenKind::kw_dd:
				return "kw_dd";
			case TokenKind::kw_dq:
				return "kw_dq";
			case TokenKind::kw_org:
				return "kw_org";
			case TokenKind::kw_virtual:
				return "kw_virtual";
			case TokenKind::kw_define:
				return "kw_define";
			case TokenKind::kw_postpone:
				return "kw_postpone";
			case TokenKind::kw_end:
				return "kw_end";

			case TokenKind::lparen:
				return "lparen";
			case TokenKind::rparen:
				return "rparen";
			case TokenKind::lbracket:
				return "lbracket";
			case TokenKind::rbracket:
				return "rbracket";
			case TokenKind::lbrace:
				return "lbrace";
			case TokenKind::rbrace:
				return "rbrace";

			case TokenKind::comma:
				return "comma";
			case TokenKind::colon:
				return "colon";
			case TokenKind::dot:
				return "dot";

			case TokenKind::at:
				return "at";
			case TokenKind::dollar:
				return "dollar";
			case TokenKind::hash:
				return "hash";
			case TokenKind::question:
				return "question";

			case TokenKind::plus:
				return "plus";
			case TokenKind::minus:
				return "minus";
			case TokenKind::star:
				return "star";
			case TokenKind::slash:
				return "slash";
			case TokenKind::percent:
				return "percent";

			case TokenKind::amp:
				return "amp";
			case TokenKind::pipe:
				return "pipe";
			case TokenKind::caret:
				return "caret";
			case TokenKind::tilde:
				return "tilde";
			case TokenKind::bang:
				return "bang";

			case TokenKind::eq:
				return "eq";
			case TokenKind::lt:
				return "lt";
			case TokenKind::gt:
				return "gt";

			case TokenKind::le:
				return "le";
			case TokenKind::ge:
				return "ge";
			case TokenKind::eqeq:
				return "eqeq";
			case TokenKind::ne:
				return "ne";

			case TokenKind::shl:
				return "shl";
			case TokenKind::shr:
				return "shr";

			case TokenKind::andand:
				return "andand";
			case TokenKind::oror:
				return "oror";
		}

		return "unknown";
	}

} // namespace yasme::lex

#endif /* YASME_LEX_TOKENS_HH */
