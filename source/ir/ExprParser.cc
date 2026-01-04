#include <cstdint>
#include <utility>
#include <yasme/Diagnostics.hh>
#include <yasme/ir/ExprParser.hh>
#include <yasme/macro/TokenSlice.hh>
#include <yasme/support/SourceManager.hh>

namespace yasme::ir
{
	ExprParser::ExprParser(CurFn cur, AdvanceFn advance, ErrorFn error)
		: m_cur(std::move(cur)), m_advance(std::move(advance)), m_error(std::move(error))
	{
	}

	bool ExprParser::is(lex::TokenKind k) const
	{
		return m_cur().is(k);
	}

	bool ExprParser::accept(lex::TokenKind k)
	{
		if (!is(k))
			return false;

		m_advance();
		return true;
	}

	lex::Token ExprParser::consume()
	{
		auto t = m_cur();
		m_advance();
		return t;
	}

	bool ExprParser::expect(lex::TokenKind k, std::string_view message)
	{
		if (is(k))
		{
			m_advance();
			return true;
		}

		m_error(m_cur().span, std::string(message));
		return false;
	}

	std::optional<BinaryOp> ExprParser::token_to_binary_op(lex::TokenKind k) const noexcept
	{
		switch (k)
		{
			case lex::TokenKind::plus:
				return BinaryOp::add;
			case lex::TokenKind::minus:
				return BinaryOp::sub;
			case lex::TokenKind::star:
				return BinaryOp::mul;
			case lex::TokenKind::slash:
				return BinaryOp::div;
			case lex::TokenKind::percent:
				return BinaryOp::mod;

			case lex::TokenKind::shl:
				return BinaryOp::shl;
			case lex::TokenKind::shr:
				return BinaryOp::shr;

			case lex::TokenKind::amp:
				return BinaryOp::bit_and;
			case lex::TokenKind::pipe:
				return BinaryOp::bit_or;
			case lex::TokenKind::caret:
				return BinaryOp::bit_xor;

			case lex::TokenKind::andand:
				return BinaryOp::log_and;
			case lex::TokenKind::oror:
				return BinaryOp::log_or;

			case lex::TokenKind::eqeq:
				return BinaryOp::eq;
			case lex::TokenKind::ne:
				return BinaryOp::ne;
			case lex::TokenKind::lt:
				return BinaryOp::lt;
			case lex::TokenKind::le:
				return BinaryOp::le;
			case lex::TokenKind::gt:
				return BinaryOp::gt;
			case lex::TokenKind::ge:
				return BinaryOp::ge;

			case lex::TokenKind::hash:
				return BinaryOp::concat;

			default:
				return std::nullopt;
		}
	}

	std::size_t ExprParser::precedence(BinaryOp op) const noexcept
	{
		switch (op)
		{
			case BinaryOp::log_or:
				return 1;
			case BinaryOp::log_and:
				return 2;

			case BinaryOp::concat:
				return 3;

			case BinaryOp::bit_or:
				return 4;
			case BinaryOp::bit_xor:
				return 5;
			case BinaryOp::bit_and:
				return 6;

			case BinaryOp::eq:
			case BinaryOp::ne:
				return 7;

			case BinaryOp::lt:
			case BinaryOp::le:
			case BinaryOp::gt:
			case BinaryOp::ge:
				return 8;

			case BinaryOp::shl:
			case BinaryOp::shr:
				return 9;

			case BinaryOp::add:
			case BinaryOp::sub:
				return 10;

			case BinaryOp::mul:
			case BinaryOp::div:
			case BinaryOp::mod:
				return 11;
		}

		return 0;
	}

	bool ExprParser::is_right_associative(BinaryOp) const noexcept
	{
		return false;
	}

	Expr ExprParser::parse_expr(std::size_t min_prec)
	{
		auto lhs = parse_unary();

		for (;;)
		{
			if (is(lex::TokenKind::newline) || is(lex::TokenKind::comma)
				|| is(lex::TokenKind::rparen) || is(lex::TokenKind::eof)
				|| is(lex::TokenKind::kw_end))
				break;

			auto op_maybe = token_to_binary_op(m_cur().kind);
			if (!op_maybe)
				break;

			auto op = *op_maybe;
			auto prec = precedence(op);

			if (prec < min_prec)
				break;

			auto op_tok = consume();

			auto rhs_min = prec + (is_right_associative(op) ? 0 : 1);
			auto rhs = parse_expr(rhs_min);

			auto s = merge_spans(lhs.span, rhs.span);
			lhs = make_binary(s, op, std::move(lhs), std::move(rhs));
			(void)op_tok;
		}

		return lhs;
	}

	Expr ExprParser::parse_unary()
	{
		if (accept(lex::TokenKind::plus))
		{
			auto rhs = parse_unary();
			auto s = merge_spans(rhs.span, rhs.span);
			return make_unary(s, UnaryOp::plus, std::move(rhs));
		}

		if (accept(lex::TokenKind::minus))
		{
			auto rhs = parse_unary();
			auto s = merge_spans(rhs.span, rhs.span);
			return make_unary(s, UnaryOp::minus, std::move(rhs));
		}

		if (accept(lex::TokenKind::tilde))
		{
			auto rhs = parse_unary();
			auto s = merge_spans(rhs.span, rhs.span);
			return make_unary(s, UnaryOp::bit_not, std::move(rhs));
		}

		if (accept(lex::TokenKind::bang))
		{
			auto rhs = parse_unary();
			auto s = merge_spans(rhs.span, rhs.span);
			return make_unary(s, UnaryOp::log_not, std::move(rhs));
		}

		if (accept(lex::TokenKind::at))
		{
			auto at_span = m_cur().span;

			if (accept(lex::TokenKind::dollar))
			{
				auto s = merge_spans(at_span, m_cur().span);
				return make_builtin(s, BuiltinKind::stream_offset);
			}

			auto rhs = parse_unary();
			auto s = merge_spans(at_span, rhs.span);
			return make_unary(s, UnaryOp::at, std::move(rhs));
		}

		return parse_primary();
	}

	Expr ExprParser::parse_primary()
	{
		if (is(lex::TokenKind::integer))
		{
			auto t = consume();
			return make_int(t);
		}

		if (is(lex::TokenKind::string) || is(lex::TokenKind::char_literal))
		{
			auto t = consume();
			return make_str(t);
		}

		if (is(lex::TokenKind::identifier))
		{
			auto t = consume();
			return make_ident(t);
		}

		if (accept(lex::TokenKind::dollar))
		{
			auto s = m_cur().span;
			return make_builtin(s, BuiltinKind::dollar_address);
		}

		if (accept(lex::TokenKind::lparen))
		{
			auto e = parse_expr();
			if (!expect(lex::TokenKind::rparen, "expected ')'"))
				return e;
			return e;
		}

		m_error(m_cur().span, "expected expression");
		auto bad = m_cur();
		m_advance();
		return Expr(bad.span, ExprInt{0});
	}

	Expr ExprParser::make_ident(lex::Token const& t) const
	{
		return Expr(t.span, ExprIdent{std::string(t.lexeme)});
	}

	Expr ExprParser::make_int(lex::Token const& t) const
	{
		if (t.integer.overflow)
		{
			auto msg = std::string("integer literal overflow: ") + std::string(t.lexeme);
			m_error(t.span, std::move(msg));
		}

		auto v = static_cast<std::int64_t>(t.integer.value);
		return Expr(t.span, ExprInt{v});
	}

	Expr ExprParser::make_str(lex::Token const& t) const
	{
		return Expr(t.span, ExprStr{unquote(t.lexeme)});
	}

	Expr ExprParser::make_builtin(SourceSpan s, BuiltinKind k) const
	{
		return Expr(s, ExprBuiltin{k});
	}

	Expr ExprParser::make_unary(SourceSpan s, UnaryOp op, Expr rhs) const
	{
		ExprUnary u{};
		u.op = op;
		u.rhs = std::make_unique<Expr>(std::move(rhs));
		return Expr(s, std::move(u));
	}

	Expr ExprParser::make_binary(SourceSpan s, BinaryOp op, Expr lhs, Expr rhs) const
	{
		ExprBinary b{};
		b.op = op;
		b.lhs = std::make_unique<Expr>(std::move(lhs));
		b.rhs = std::make_unique<Expr>(std::move(rhs));
		return Expr(s, std::move(b));
	}

	SourceSpan ExprParser::merge_spans(SourceSpan a, SourceSpan b) noexcept
	{
		if (a.id == 0)
			return b;
		if (b.id == 0)
			return a;
		if (a.id != b.id)
			return a;

		SourceSpan out{};
		out.id = a.id;
		out.begin = a.begin;
		out.end = b.end;
		return out;
	}

	std::string ExprParser::unquote(std::string_view s)
	{
		if (s.size() >= 2)
		{
			auto q0 = s.front();
			auto q1 = s.back();
			if ((q0 == '"' && q1 == '"') || (q0 == '\'' && q1 == '\''))
				return std::string(s.substr(1, s.size() - 2));
		}
		return std::string(s);
	}

	Expr parse_expr_from_tokens(SourceManager const& sources,
								FileId file,
								macro::TokenSlice slice,
								Diagnostics& diag)
	{
		struct SliceStream
		{
			macro::TokenSlice slice{};
			std::size_t index{};
			lex::Token eof{};

			[[nodiscard]] std::size_t size() const noexcept
			{
				if (!slice.begin || !slice.end)
					return 0;
				return static_cast<std::size_t>(slice.end - slice.begin);
			}

			[[nodiscard]] lex::Token const& cur() const noexcept
			{
				auto count = size();
				if (count == 0 || index >= count)
					return eof;
				return slice.begin[index];
			}

			void advance() noexcept
			{
				auto count = size();
				if (index < count)
					++index;
			}
		};

		SourceSpan fallback = slice.span;
		if (fallback.id == 0 && file != 0 && sources.has(file))
			fallback = sources.span_from_offsets(file, 0, 0);

		SliceStream stream{};
		stream.slice = slice;
		stream.eof.kind = lex::TokenKind::eof;
		stream.eof.span = fallback;

		auto cur = [&stream]() -> lex::Token const& { return stream.cur(); };
		auto advance = [&stream]() { stream.advance(); };
		auto error = [&diag](SourceSpan span, std::string message) {
			Diagnostic d{};
			d.level = DiagnosticLevel::error;
			d.message = std::move(message);
			d.primary = span;
			diag.emit(d);
		};

		ExprParser parser(cur, advance, error);
		return parser.parse_expr();
	}

} // namespace yasme::ir
