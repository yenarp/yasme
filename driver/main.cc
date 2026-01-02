#include <cstddef>
#include <iostream>
#include <string>
#include <string_view>
#include <vector>
#include <yasme/lex/lexer.hh>
#include <yasme/lex/tokens.hh>
#include <yasme/support/SourceManager.hh>

namespace
{
	using yasme::FileId;
	using yasme::SourceManager;

	using yasme::lex::Lexer;
	using yasme::lex::LexerOptions;
	using yasme::lex::NumberBase;
	using yasme::lex::Token;
	using yasme::lex::token_kind_name;
	using yasme::lex::TokenKind;

	struct ExpectedToken
	{
		TokenKind kind{};
		std::string_view lexeme{};

		bool check_lexeme{true};

		bool check_int{};
		std::uint64_t int_value{};
		NumberBase int_base{NumberBase::decimal};
	};

	struct ExpectedError
	{
		std::string_view contains{};
	};

	struct TestCase
	{
		std::string_view name{};
		std::string source{};
		LexerOptions opt{};

		std::vector<ExpectedToken> tokens{};
		std::vector<ExpectedError> errors{};
	};

	static std::string_view number_base_name(NumberBase b) noexcept
	{
		switch (b)
		{
			case NumberBase::binary:
				return "bin";
			case NumberBase::octal:
				return "oct";
			case NumberBase::decimal:
				return "dec";
			case NumberBase::hexadecimal:
				return "hex";
		}
		return "unknown";
	}

	static void dump_token(std::ostream& os, Token const& t)
	{
		os << token_kind_name(t.kind) << " `" << t.lexeme << "`";
		if (t.kind == TokenKind::integer)
		{
			os << " (value=" << t.integer.value << " base=" << number_base_name(t.integer.base)
			   << " overflow=" << (t.integer.overflow ? "true" : "false") << ")";
		}
		os << " @ " << t.span.begin.line << ":" << t.span.begin.column;
	}

	static bool token_matches(Token const& got, ExpectedToken const& exp)
	{
		if (got.kind != exp.kind)
			return false;

		if (exp.check_lexeme && got.lexeme != exp.lexeme)
			return false;

		if (exp.check_int)
		{
			if (got.kind != TokenKind::integer)
				return false;
			if (got.integer.value != exp.int_value)
				return false;
			if (got.integer.base != exp.int_base)
				return false;
		}

		return true;
	}

	static bool error_matches(std::string_view msg, ExpectedError const& exp)
	{
		if (exp.contains.empty())
			return true;

		return msg.find(exp.contains) != std::string_view::npos;
	}

	static bool run_one(SourceManager& sm, TestCase const& tc)
	{
		auto const id = sm.add_virtual(std::string(tc.name), tc.source);

		Lexer lx(sm, id, tc.opt);

		std::vector<Token> got_tokens{};
		for (;;)
		{
			auto t = lx.next();
			got_tokens.push_back(t);
			if (t.kind == TokenKind::eof)
				break;
		}

		bool ok = true;

		auto const n = tc.tokens.size();
		auto const m = got_tokens.size();

		if (n != m)
			ok = false;

		auto const k = (n < m) ? n : m;
		for (std::size_t i = 0; i < k; ++i)
		{
			if (!token_matches(got_tokens[i], tc.tokens[i]))
			{
				ok = false;

				std::cout << "\n[" << tc.name << "] mismatch at token " << i << "\n";
				std::cout << "  expected: " << token_kind_name(tc.tokens[i].kind);
				if (tc.tokens[i].check_lexeme)
					std::cout << " `" << tc.tokens[i].lexeme << "`";
				if (tc.tokens[i].check_int)
				{
					std::cout << " (value=" << tc.tokens[i].int_value
							  << " base=" << number_base_name(tc.tokens[i].int_base) << ")";
				}
				std::cout << "\n  got:      ";
				dump_token(std::cout, got_tokens[i]);
				std::cout << "\n";
				break;
			}
		}

		auto got_errs = lx.errors();
		if (got_errs.size() != tc.errors.size())
		{
			ok = false;
			std::cout << "\n[" << tc.name << "] error count mismatch\n";
			std::cout << "  expected: " << tc.errors.size() << "\n";
			std::cout << "  got:      " << got_errs.size() << "\n";
		}

		auto const ek = (tc.errors.size() < got_errs.size()) ? tc.errors.size() : got_errs.size();
		for (std::size_t i = 0; i < ek; ++i)
		{
			if (!error_matches(got_errs[i].message, tc.errors[i]))
			{
				ok = false;
				std::cout << "\n[" << tc.name << "] error " << i << " mismatch\n";
				std::cout << "  expected contains: `" << tc.errors[i].contains << "`\n";
				std::cout << "  got:              `" << got_errs[i].message << "`\n";
				break;
			}
		}

		if (!ok)
		{
			std::cout << "\n[" << tc.name << "] --- got tokens (" << got_tokens.size() << ") ---\n";
			for (std::size_t i = 0; i < got_tokens.size(); ++i)
			{
				std::cout << i << ": ";
				dump_token(std::cout, got_tokens[i]);
				std::cout << "\n";
			}

			if (!got_errs.empty())
			{
				std::cout << "\n[" << tc.name << "] --- lexer errors ---\n";
				for (auto const& e : got_errs)
				{
					std::cout << e.span.begin.line << ":" << e.span.begin.column << ": "
							  << e.message << "\n";
				}
			}
		}

		return ok;
	}

	static ExpectedToken tok(TokenKind k, std::string_view lex)
	{
		ExpectedToken e{};
		e.kind = k;
		e.lexeme = lex;
		return e;
	}

	static ExpectedToken tokk(TokenKind k)
	{
		ExpectedToken e{};
		e.kind = k;
		e.check_lexeme = false;
		return e;
	}

	static ExpectedToken inttok(std::string_view lex, std::uint64_t v, NumberBase b)
	{
		ExpectedToken e{};
		e.kind = TokenKind::integer;
		e.lexeme = lex;
		e.check_int = true;
		e.int_value = v;
		e.int_base = b;
		return e;
	}

} // namespace

int main()
{
	using NB = NumberBase;

	SourceManager sm;

	std::vector<TestCase> tests{};

	{
		TestCase tc{};
		tc.name = "blocks_virtual_postpone";
		tc.opt.emit_newlines = true;
		tc.opt.case_insensitive_keywords = true;
		tc.opt.enable_c_comments = true;

		tc.source = "org 0x100\n"
					"db 1 ; emitted to output\n"
					"\n"
					"virtual block_name\n"
					"    db 0x90, 10, 'A' ; inside virtual stream\n"
					"    dw 0b1010_0101\n"
					"end virtual\n"
					"\n"
					"postpone\n"
					"    dd 200h\n"
					"end postpone\n"
					"\n"
					"postpone !\n"
					"    dq 101b\n"
					"end postpone\n"
					"\n"
					"define foo, 1\n"
					"end\n";

		tc.tokens = {
			tok(TokenKind::kw_org, "org"),
			inttok("0x100", 256, NB::hexadecimal),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_db, "db"),
			inttok("1", 1, NB::decimal),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_virtual, "virtual"),
			tok(TokenKind::identifier, "block_name"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_db, "db"),
			inttok("0x90", 144, NB::hexadecimal),
			tok(TokenKind::comma, ","),
			inttok("10", 10, NB::decimal),
			tok(TokenKind::comma, ","),
			tok(TokenKind::char_literal, "'A'"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_dw, "dw"),
			inttok("0b1010_0101", 165, NB::binary),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_end, "end"),
			tok(TokenKind::kw_virtual, "virtual"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_postpone, "postpone"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_dd, "dd"),
			inttok("200h", 512, NB::hexadecimal),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_end, "end"),
			tok(TokenKind::kw_postpone, "postpone"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_postpone, "postpone"),
			tok(TokenKind::bang, "!"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_dq, "dq"),
			inttok("101b", 5, NB::binary),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_end, "end"),
			tok(TokenKind::kw_postpone, "postpone"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_define, "define"),
			tok(TokenKind::identifier, "foo"),
			tok(TokenKind::comma, ","),
			inttok("1", 1, NB::decimal),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_end, "end"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::eof, ""),
		};

		tests.push_back(std::move(tc));
	}

	{
		TestCase tc{};
		tc.name = "case_insensitive_keywords";
		tc.opt.emit_newlines = true;
		tc.opt.case_insensitive_keywords = true;
		tc.opt.enable_c_comments = true;

		tc.source = "OrG 0x10\n"
					"ViRtUaL Name\n"
					"EnD vIrTuAl\n"
					"PoStPoNe !\n"
					"EnD pOsTpOnE\n"
					"eNd\n";

		tc.tokens = {
			tok(TokenKind::kw_org, "OrG"),
			inttok("0x10", 16, NB::hexadecimal),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_virtual, "ViRtUaL"),
			tok(TokenKind::identifier, "Name"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_end, "EnD"),
			tok(TokenKind::kw_virtual, "vIrTuAl"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_postpone, "PoStPoNe"),
			tok(TokenKind::bang, "!"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_end, "EnD"),
			tok(TokenKind::kw_postpone, "pOsTpOnE"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_end, "eNd"),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::eof, ""),
		};

		tests.push_back(std::move(tc));
	}

	{
		TestCase tc{};
		tc.name = "operators";
		tc.opt.emit_newlines = true;
		tc.opt.case_insensitive_keywords = true;
		tc.opt.enable_c_comments = true;

		tc.source = "a<<b>>c<=d>=e==f!=g&&h||i\n";

		tc.tokens = {
			tok(TokenKind::identifier, "a"), tok(TokenKind::shl, "<<"),
			tok(TokenKind::identifier, "b"), tok(TokenKind::shr, ">>"),
			tok(TokenKind::identifier, "c"), tok(TokenKind::le, "<="),
			tok(TokenKind::identifier, "d"), tok(TokenKind::ge, ">="),
			tok(TokenKind::identifier, "e"), tok(TokenKind::eqeq, "=="),
			tok(TokenKind::identifier, "f"), tok(TokenKind::ne, "!="),
			tok(TokenKind::identifier, "g"), tok(TokenKind::andand, "&&"),
			tok(TokenKind::identifier, "h"), tok(TokenKind::oror, "||"),
			tok(TokenKind::identifier, "i"), tok(TokenKind::newline, "\n"),
			tok(TokenKind::eof, ""),
		};

		tests.push_back(std::move(tc));
	}

	{
		TestCase tc{};
		tc.name = "dot_vs_identifier";
		tc.opt.emit_newlines = true;
		tc.opt.case_insensitive_keywords = true;
		tc.opt.enable_c_comments = true;

		tc.source = "foo.bar .local\n"
					".\n";

		tc.tokens = {
			tok(TokenKind::identifier, "foo.bar"),
			tok(TokenKind::identifier, ".local"),
			tok(TokenKind::newline, "\n"),
			tok(TokenKind::dot, "."),
			tok(TokenKind::newline, "\n"),
			tok(TokenKind::eof, ""),
		};

		tests.push_back(std::move(tc));
	}

	{
		TestCase tc{};
		tc.name = "numbers_various";
		tc.opt.emit_newlines = true;
		tc.opt.case_insensitive_keywords = true;
		tc.opt.enable_c_comments = true;

		tc.source = "db 200h, 101b, 77o, 42d, 0o77, 0b1_0_1, 0xdead_beef\n";

		tc.tokens = {
			tok(TokenKind::kw_db, "db"),
			inttok("200h", 512, NB::hexadecimal),
			tok(TokenKind::comma, ","),
			inttok("101b", 5, NB::binary),
			tok(TokenKind::comma, ","),
			inttok("77o", 63, NB::octal),
			tok(TokenKind::comma, ","),
			inttok("42d", 42, NB::decimal),
			tok(TokenKind::comma, ","),
			inttok("0o77", 63, NB::octal),
			tok(TokenKind::comma, ","),
			inttok("0b1_0_1", 5, NB::binary),
			tok(TokenKind::comma, ","),
			inttok("0xdead_beef", 3735928559, NB::hexadecimal),
			tok(TokenKind::newline, "\n"),
			tok(TokenKind::eof, ""),
		};

		tests.push_back(std::move(tc));
	}

	{
		TestCase tc{};
		tc.name = "semicolon_is_comment";
		tc.opt.emit_newlines = true;
		tc.opt.case_insensitive_keywords = true;
		tc.opt.enable_c_comments = true;

		tc.source = "db 1;comment\n"
					"db 2 ; comment\n";

		tc.tokens = {
			tok(TokenKind::kw_db, "db"),
			inttok("1", 1, NB::decimal),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_db, "db"),
			inttok("2", 2, NB::decimal),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::eof, ""),
		};

		tests.push_back(std::move(tc));
	}

	{
		TestCase tc{};
		tc.name = "c_comments_enabled";
		tc.opt.emit_newlines = true;
		tc.opt.case_insensitive_keywords = true;
		tc.opt.enable_c_comments = true;

		tc.source = "db 1 // comment\n"
					"db 2 /* block */\n";

		tc.tokens = {
			tok(TokenKind::kw_db, "db"),
			inttok("1", 1, NB::decimal),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::kw_db, "db"),
			inttok("2", 2, NB::decimal),
			tok(TokenKind::newline, "\n"),

			tok(TokenKind::eof, ""),
		};

		tests.push_back(std::move(tc));
	}

	{
		TestCase tc{};
		tc.name = "c_comments_disabled";
		tc.opt.emit_newlines = true;
		tc.opt.case_insensitive_keywords = true;
		tc.opt.enable_c_comments = false;

		tc.source = "db 1 // not comment\n";

		tc.tokens = {
			tok(TokenKind::kw_db, "db"),
			inttok("1", 1, NB::decimal),
			tok(TokenKind::slash, "/"),
			tok(TokenKind::slash, "/"),
			tok(TokenKind::identifier, "not"),
			tok(TokenKind::identifier, "comment"),
			tok(TokenKind::newline, "\n"),
			tok(TokenKind::eof, ""),
		};

		tests.push_back(std::move(tc));
	}

	{
		TestCase tc{};
		tc.name = "unterminated_string";
		tc.opt.emit_newlines = true;
		tc.opt.case_insensitive_keywords = true;
		tc.opt.enable_c_comments = true;

		tc.source = "define s, \"abc\n"
					"end\n";

		tc.tokens = {
			tok(TokenKind::kw_define, "define"),
			tok(TokenKind::identifier, "s"),
			tok(TokenKind::comma, ","),
			tok(TokenKind::string, "\"abc"),
			tok(TokenKind::newline, "\n"),
			tok(TokenKind::kw_end, "end"),
			tok(TokenKind::newline, "\n"),
			tok(TokenKind::eof, ""),
		};

		tc.errors = {
			{"unterminated string literal"},
		};

		tests.push_back(std::move(tc));
	}

	{
		TestCase tc{};
		tc.name = "unterminated_block_comment";
		tc.opt.emit_newlines = true;
		tc.opt.case_insensitive_keywords = true;
		tc.opt.enable_c_comments = true;

		tc.source = "db 1 /* oops\n";

		tc.tokens = {
			tok(TokenKind::kw_db, "db"),
			inttok("1", 1, NB::decimal),
			tok(TokenKind::eof, ""),
		};

		tc.errors = {
			{"unterminated block comment"},
		};

		tests.push_back(std::move(tc));
	}

	{
		TestCase tc{};
		tc.name = "crlf_newlines";
		tc.opt.emit_newlines = true;
		tc.opt.case_insensitive_keywords = true;
		tc.opt.enable_c_comments = true;

		tc.source = "org 0x1\r\ndb 2\r\n";

		tc.tokens = {
			tok(TokenKind::kw_org, "org"),
			inttok("0x1", 1, NB::hexadecimal),
			tok(TokenKind::newline, "\r\n"),

			tok(TokenKind::kw_db, "db"),
			inttok("2", 2, NB::decimal),
			tok(TokenKind::newline, "\r\n"),

			tok(TokenKind::eof, ""),
		};

		tests.push_back(std::move(tc));
	}

	bool all_ok = true;

	for (auto const& tc : tests)
	{
		auto ok = run_one(sm, tc);
		if (!ok)
			all_ok = false;

		std::cout << "[" << tc.name << "] " << (ok ? "ok" : "failed") << "\n";
	}

	std::cout << "\n--- summary ---\n";
	std::cout << (all_ok ? "LEXER TESTS OK\n" : "LEXER TESTS FAILED\n");
	return all_ok ? 0 : 1;
}
