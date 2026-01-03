#include <limits>
#include <yasme/lex/Lexer.hh>
#include <yasme/support/Ctype.hh>

namespace yasme::lex
{
	static bool ieq(std::string_view a, std::string_view b) noexcept
	{
		if (a.size() != b.size())
			return false;

		for (std::size_t i = 0; i < a.size(); ++i)
		{
			if (ascii_tolower(a[i]) != ascii_tolower(b[i]))
				return false;
		}

		return true;
	}

	static TokenKind classify_keyword(std::string_view s, bool case_insensitive) noexcept
	{
		auto eq = [&](std::string_view kw) noexcept -> bool {
			if (case_insensitive)
				return ieq(s, kw);

			return s == kw;
		};

		if (eq("db"))
			return TokenKind::kw_db;
		if (eq("dw"))
			return TokenKind::kw_dw;
		if (eq("dd"))
			return TokenKind::kw_dd;
		if (eq("dq"))
			return TokenKind::kw_dq;
		if (eq("org"))
			return TokenKind::kw_org;
		if (eq("virtual"))
			return TokenKind::kw_virtual;
		if (eq("define"))
			return TokenKind::kw_define;
		if (eq("postpone"))
			return TokenKind::kw_postpone;
		if (eq("end"))
			return TokenKind::kw_end;

		return TokenKind::identifier;
	}

	static bool is_digit_for_base(char c, NumberBase base) noexcept
	{
		if (c == '_')
			return true;

		if (is_ascii_digit(c))
			return true;

		if (base == NumberBase::hexadecimal)
		{
			auto lc = ascii_tolower(c);
			return lc >= 'a' && lc <= 'f';
		}

		return false;
	}

	static int digit_value(char c) noexcept
	{
		if (c >= '0' && c <= '9')
			return c - '0';

		auto lc = ascii_tolower(c);
		if (lc >= 'a' && lc <= 'f')
			return lc - 'a' + 10;

		return -1;
	}

	Lexer::Lexer(SourceManager const& sources, FileId file, LexerOptions opt) noexcept
		: m_sources(&sources), m_file(file), m_input(sources.content(file)), m_opt(opt)
	{
	}

	void Lexer::reset(FileByte offset) noexcept
	{
		m_off = offset;
		m_has_peek = false;
		m_peeked = {};
		m_errors.clear();
	}

	Token Lexer::peek()
	{
		if (!m_has_peek)
		{
			m_peeked = lex_one();
			m_has_peek = true;
		}
		return m_peeked;
	}

	Token Lexer::next()
	{
		if (m_has_peek)
		{
			m_has_peek = false;
			return m_peeked;
		}
		return lex_one();
	}

	bool Lexer::at_end() const noexcept
	{
		return m_off >= m_input.size();
	}

	char Lexer::cur() const noexcept
	{
		if (at_end())
			return '\0';

		return m_input[m_off];
	}

	char Lexer::peek_char(FileByte rel) const noexcept
	{
		auto idx = m_off + rel;
		if (idx >= m_input.size())
			return '\0';

		return m_input[idx];
	}

	void Lexer::advance(FileByte n) noexcept
	{
		m_off += n;
		if (m_off > m_input.size())
			m_off = m_input.size();
	}

	Token Lexer::make(TokenKind kind, FileByte begin, FileByte end) const noexcept
	{
		Token t{};
		t.kind = kind;
		t.span = m_sources->span_from_offsets(m_file, begin, end);
		if (begin <= end && end <= m_input.size())
			t.lexeme = m_input.substr(begin, end - begin);

		return t;
	}

	void Lexer::add_error(FileByte begin, FileByte end, std::string message)
	{
		LexError e{};
		e.span = m_sources->span_from_offsets(m_file, begin, end);
		e.message = std::move(message);
		m_errors.push_back(std::move(e));
	}

	void Lexer::skip_spaces_and_comments()
	{
		for (;;)
		{
			if (at_end())
				return;

			auto c = cur();

			if (c == '\n' || c == '\r')
			{
				if (m_opt.emit_newlines)
					return;

				if (c == '\r' && peek_char() == '\n')
					advance(2);
				else
					advance();

				continue;
			}

			if (c == ' ' || c == '\t' || c == '\v' || c == '\f')
			{
				advance();
				continue;
			}

			if (c == ';')
			{
				advance();
				while (!at_end())
				{
					auto cc = cur();
					if (cc == '\n' || cc == '\r')
						break;

					advance();
				}
				continue;
			}

			if (m_opt.enable_c_comments && c == '/' && peek_char() == '/')
			{
				advance(2);
				while (!at_end())
				{
					auto cc = cur();
					if (cc == '\n' || cc == '\r')
						break;

					advance();
				}
				continue;
			}

			if (m_opt.enable_c_comments && c == '/' && peek_char() == '*')
			{
				auto begin = m_off;
				advance(2);
				while (!at_end())
				{
					if (cur() == '*' && peek_char() == '/')
					{
						advance(2);
						break;
					}
					advance();
				}

				if (at_end())
					add_error(begin, m_off, "unterminated block comment");

				continue;
			}

			return;
		}
	}

	Token Lexer::lex_string_like(FileByte begin, char quote, TokenKind kind)
	{
		advance();

		while (!at_end())
		{
			auto c = cur();

			if (c == '\n' || c == '\r')
			{
				add_error(begin, m_off, "unterminated string literal");
				return make(kind, begin, m_off);
			}

			if (c == quote)
			{
				advance();
				return make(kind, begin, m_off);
			}

			if (c == '\\')
			{
				advance();
				if (!at_end())
				{
					auto esc = cur();
					if (esc != '\n' && esc != '\r')
						advance();
				}
				continue;
			}

			advance();
		}

		add_error(begin, m_off, "unterminated string literal");
		return make(kind, begin, m_off);
	}

	Token Lexer::lex_identifier(FileByte begin)
	{
		advance();

		while (!at_end())
		{
			auto c = cur();
			if (!is_ident_continue(c))
				break;
			advance();
		}

		auto end = m_off;
		auto t = make(TokenKind::identifier, begin, end);

		t.kind = classify_keyword(t.lexeme, m_opt.case_insensitive_keywords);
		return t;
	}

	Token Lexer::lex_integer(FileByte begin)
	{
		auto base = NumberBase::decimal;

		if (cur() == '0')
		{
			auto n = ascii_tolower(peek_char());
			if (n == 'x')
			{
				base = NumberBase::hexadecimal;
				advance(2);
				while (!at_end() && is_digit_for_base(cur(), base))
					advance();
			}
			else if (n == 'b')
			{
				base = NumberBase::binary;
				advance(2);
				while (!at_end())
				{
					auto c = cur();
					if (c == '_' || c == '0' || c == '1')
					{
						advance();
						continue;
					}
					break;
				}
			}
			else if (n == 'o')
			{
				base = NumberBase::octal;
				advance(2);
				while (!at_end())
				{
					auto c = cur();
					if (c == '_' || (c >= '0' && c <= '7'))
					{
						advance();
						continue;
					}
					break;
				}
			}
			else
			{
				advance();
				while (!at_end() && (is_ascii_digit(cur()) || cur() == '_'))
					advance();
			}
		}
		else
		{
			while (!at_end())
			{
				auto c = cur();
				if (is_ascii_digit(c) || c == '_')
				{
					advance();
					continue;
				}

				auto lc = ascii_tolower(c);
				if (lc >= 'a' && lc <= 'f')
				{
					advance();
					continue;
				}

				break;
			}

			auto suf = ascii_tolower(cur());
			if (suf == 'h' || suf == 'b' || suf == 'o' || suf == 'q' || suf == 'd')
				advance();
		}

		auto end = m_off;
		auto t = make(TokenKind::integer, begin, end);

		auto s = t.lexeme;

		if (s.size() >= 2 && s[0] == '0')
		{
			auto p = ascii_tolower(s[1]);
			if (p == 'x')
			{
				base = NumberBase::hexadecimal;
				s.remove_prefix(2);
			}
			else if (p == 'b')
			{
				base = NumberBase::binary;
				s.remove_prefix(2);
			}
			else if (p == 'o')
			{
				base = NumberBase::octal;
				s.remove_prefix(2);
			}
		}

		if (!s.empty())
		{
			auto suf = ascii_tolower(s.back());
			if (suf == 'h')
			{
				base = NumberBase::hexadecimal;
				s.remove_suffix(1);
			}
			else if (suf == 'b')
			{
				base = NumberBase::binary;
				s.remove_suffix(1);
			}
			else if (suf == 'o' || suf == 'q')
			{
				base = NumberBase::octal;
				s.remove_suffix(1);
			}
			else if (suf == 'd')
			{
				base = NumberBase::decimal;
				s.remove_suffix(1);
			}
		}

		std::uint64_t value{};
		bool overflow{};
		bool bad_digit{};

		auto maxv = std::numeric_limits<std::uint64_t>::max();
		auto b = static_cast<std::uint64_t>(base);

		for (char c : s)
		{
			if (c == '_')
				continue;

			auto d = digit_value(c);
			if (d < 0 || static_cast<std::uint64_t>(d) >= b)
			{
				bad_digit = true;
				break;
			}

			auto ud = static_cast<std::uint64_t>(d);

			if (value > (maxv - ud) / b)
			{
				overflow = true;
				value = maxv;
				continue;
			}

			value = value * b + ud;
		}

		t.integer.value = value;
		t.integer.base = base;
		t.integer.overflow = overflow;

		if (bad_digit)
			add_error(begin, end, "invalid digit in integer literal");
		else if (overflow)
			add_error(begin, end, "integer literal overflows 64-bit");

		return t;
	}

	Token Lexer::lex_one()
	{
		for (;;)
		{
			skip_spaces_and_comments();

			if (at_end())
				return make(TokenKind::eof, m_off, m_off);

			auto begin = m_off;
			auto c = cur();

			if (c == '\n' || c == '\r')
			{
				if (!m_opt.emit_newlines)
				{
					if (c == '\r' && peek_char() == '\n')
						advance(2);
					else
						advance();
					continue;
				}

				if (c == '\r' && peek_char() == '\n')
				{
					advance(2);
					return make(TokenKind::newline, begin, m_off);
				}

				advance();
				return make(TokenKind::newline, begin, m_off);
			}

			if (c == '"')
				return lex_string_like(begin, '"', TokenKind::string);

			if (c == '\'')
				return lex_string_like(begin, '\'', TokenKind::char_literal);

			if (is_ascii_digit(c))
				return lex_integer(begin);

			if (c == '.' && is_ident_start(peek_char()))
				return lex_identifier(begin);

			if (is_ascii_alpha(c) || c == '_')
				return lex_identifier(begin);

			auto n = peek_char();

			switch (c)
			{
				case '(':
					advance();
					return make(TokenKind::lparen, begin, m_off);
				case ')':
					advance();
					return make(TokenKind::rparen, begin, m_off);
				case '[':
					advance();
					return make(TokenKind::lbracket, begin, m_off);
				case ']':
					advance();
					return make(TokenKind::rbracket, begin, m_off);
				case '{':
					advance();
					return make(TokenKind::lbrace, begin, m_off);
				case '}':
					advance();
					return make(TokenKind::rbrace, begin, m_off);

				case ',':
					advance();
					return make(TokenKind::comma, begin, m_off);
				case ':':
					advance();
					return make(TokenKind::colon, begin, m_off);
				case '.':
					advance();
					return make(TokenKind::dot, begin, m_off);

				case '@':
					advance();
					return make(TokenKind::at, begin, m_off);
				case '$':
					advance();
					return make(TokenKind::dollar, begin, m_off);
				case '#':
					advance();
					return make(TokenKind::hash, begin, m_off);
				case '?':
					advance();
					return make(TokenKind::question, begin, m_off);

				case '+':
					advance();
					return make(TokenKind::plus, begin, m_off);
				case '-':
					advance();
					return make(TokenKind::minus, begin, m_off);
				case '*':
					advance();
					return make(TokenKind::star, begin, m_off);
				case '/':
					advance();
					return make(TokenKind::slash, begin, m_off);
				case '%':
					advance();
					return make(TokenKind::percent, begin, m_off);

				case '~':
					advance();
					return make(TokenKind::tilde, begin, m_off);

				case '!':
					if (n == '=')
					{
						advance(2);
						return make(TokenKind::ne, begin, m_off);
					}
					advance();
					return make(TokenKind::bang, begin, m_off);

				case '&':
					if (n == '&')
					{
						advance(2);
						return make(TokenKind::andand, begin, m_off);
					}
					advance();
					return make(TokenKind::amp, begin, m_off);

				case '|':
					if (n == '|')
					{
						advance(2);
						return make(TokenKind::oror, begin, m_off);
					}
					advance();
					return make(TokenKind::pipe, begin, m_off);

				case '^':
					advance();
					return make(TokenKind::caret, begin, m_off);

				case '=':
					if (n == '=')
					{
						advance(2);
						return make(TokenKind::eqeq, begin, m_off);
					}
					advance();
					return make(TokenKind::eq, begin, m_off);

				case '<':
					if (n == '=')
					{
						advance(2);
						return make(TokenKind::le, begin, m_off);
					}
					if (n == '<')
					{
						advance(2);
						return make(TokenKind::shl, begin, m_off);
					}
					advance();
					return make(TokenKind::lt, begin, m_off);

				case '>':
					if (n == '=')
					{
						advance(2);
						return make(TokenKind::ge, begin, m_off);
					}
					if (n == '>')
					{
						advance(2);
						return make(TokenKind::shr, begin, m_off);
					}
					advance();
					return make(TokenKind::gt, begin, m_off);
			}

			advance();
			add_error(begin, m_off, "invalid character");
			return make(TokenKind::invalid, begin, m_off);
		}
	}

} // namespace yasme::lex
