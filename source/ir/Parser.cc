#include <cstdint>
#include <utility>
#include <yasme/ir/Parser.hh>

namespace yasme::ir
{
	Parser::Parser(SourceManager const& sources,
				   FileId file,
				   lex::LexerOptions lex_opt,
				   ParserOptions opt) noexcept
		: m_lex(sources, file, lex_opt), m_opt(opt)
	{
		m_cur = m_lex.next();
		m_next = m_lex.next();
	}

	ParserResult Parser::parse_program()
	{
		Program p{};

		skip_newlines();

		while (!at_end())
		{
			auto st = parse_stmt();
			if (st)
				p.stmts.push_back(std::move(st));
			else
				recover_to_newline();

			skip_newlines();
		}

		ParserResult r{};
		r.program = std::move(p);
		r.errors = std::move(m_errors);
		return r;
	}

	bool Parser::at_end() const noexcept
	{
		return cur().is(lex::TokenKind::eof);
	}

	void Parser::advance() noexcept
	{
		m_cur = std::move(m_next);
		m_next = m_lex.next();
	}

	bool Parser::is(lex::TokenKind k) const noexcept
	{
		return cur().is(k);
	}

	bool Parser::is_next(lex::TokenKind k) const noexcept
	{
		return next_tok().is(k);
	}

	bool Parser::accept(lex::TokenKind k) noexcept
	{
		if (!is(k))
			return false;

		advance();
		return true;
	}

	lex::Token Parser::consume() noexcept
	{
		auto t = cur();
		advance();
		return t;
	}

	bool Parser::expect(lex::TokenKind k, std::string_view message)
	{
		if (is(k))
		{
			advance();
			return true;
		}

		add_error(cur().span, std::string(message));
		return false;
	}

	void Parser::skip_newlines() noexcept
	{
		while (accept(lex::TokenKind::newline))
		{
		}
	}

	void Parser::add_error(SourceSpan span, std::string message)
	{
		m_errors.push_back(ParseError{span, std::move(message)});
	}

	void Parser::recover_to_newline() noexcept
	{
		while (!at_end() && !is(lex::TokenKind::newline))
			advance();

		skip_newlines();
	}

	StmtPtr Parser::parse_stmt()
	{
		if (is(lex::TokenKind::newline))
		{
			skip_newlines();
			return nullptr;
		}

		if (is(lex::TokenKind::kw_org))
			return parse_stmt_org();

		if (is(lex::TokenKind::identifier))
			return parse_stmt_label_or_assign();

		if (is(lex::TokenKind::kw_define))
			return parse_stmt_define();

		if (is(lex::TokenKind::kw_db) || is(lex::TokenKind::kw_dw) || is(lex::TokenKind::kw_dd)
			|| is(lex::TokenKind::kw_dq))
		{
			auto d = cur().kind;
			return parse_stmt_emit_data(d);
		}

		if (is(lex::TokenKind::kw_virtual))
			return parse_stmt_virtual();

		if (is(lex::TokenKind::kw_postpone))
			return parse_stmt_postpone();

		if (is(lex::TokenKind::kw_end))
			return parse_stmt_unexpected_end();

		if (is(lex::TokenKind::eof))
			return nullptr;

		add_error(cur().span, "unexpected token at start of statement");
		advance();
		return nullptr;
	}

	StmtPtr Parser::parse_stmt_org()
	{
		auto kw = consume();
		auto addr = parse_expr();

		auto s = StmtOrg{};
		s.span = kw.span;
		s.address = std::move(addr);

		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_label_or_assign()
	{
		auto name_tok = consume();
		auto name = std::string(name_tok.lexeme);

		if (accept(lex::TokenKind::colon))
		{
			auto s = StmtLabel{};
			s.span = merge_spans(name_tok.span, name_tok.span);
			s.name = std::move(name);
			return std::make_unique<Stmt>(Stmt(std::move(s)));
		}

		if (!accept(lex::TokenKind::eq))
		{
			add_error(cur().span, "expected ':' for label or '=' for assignment");
			return nullptr;
		}

		auto rhs = parse_expr();

		auto s = StmtAssign{};
		s.span = merge_spans(name_tok.span, rhs.span);
		s.name = std::move(name);
		s.rhs = std::move(rhs);

		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_define()
	{
		auto kw = consume();

		if (!is(lex::TokenKind::identifier))
		{
			add_error(cur().span, "expected identifier after 'define'");
			return nullptr;
		}

		auto name_tok = consume();
		auto name = std::string(name_tok.lexeme);

		if (!(accept(lex::TokenKind::comma) || accept(lex::TokenKind::eq)))
		{
			add_error(cur().span, "expected ',' or '=' after define name");
			return nullptr;
		}

		auto value = parse_expr();

		auto s = StmtDefine{};
		s.span = merge_spans(kw.span, value.span);
		s.name = std::move(name);
		s.value = std::move(value);

		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_emit_data(lex::TokenKind directive)
	{
		auto kw = consume();

		DataUnit unit = DataUnit::u8;
		switch (directive)
		{
			case lex::TokenKind::kw_db:
				unit = DataUnit::u8;
				break;
			case lex::TokenKind::kw_dw:
				unit = DataUnit::u16;
				break;
			case lex::TokenKind::kw_dd:
				unit = DataUnit::u32;
				break;
			case lex::TokenKind::kw_dq:
				unit = DataUnit::u64;
				break;
			default:
				break;
		}

		std::vector<Expr> items{};

		if (is(lex::TokenKind::newline) || is(lex::TokenKind::eof))
		{
			if (!m_opt.allow_empty_data_list)
				add_error(cur().span, "expected at least one item for data directive");
		}
		else
		{
			items.push_back(parse_expr());

			while (accept(lex::TokenKind::comma))
			{
				if (is(lex::TokenKind::newline) || is(lex::TokenKind::eof))
				{
					if (!m_opt.allow_trailing_commas)
						add_error(cur().span, "trailing comma not allowed here");
					break;
				}

				items.push_back(parse_expr());
			}
		}

		auto s = StmtEmitData{};
		s.span = kw.span;
		s.unit = unit;
		s.items = std::move(items);

		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_virtual()
	{
		auto kw = consume();

		std::optional<Expr> name_expr{};

		if (!is(lex::TokenKind::newline) && !is(lex::TokenKind::eof))
			name_expr = parse_expr();

		skip_newlines();

		std::vector<StmtPtr> body{};

		for (;;)
		{
			skip_newlines();

			if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_virtual))
				break;

			if (is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'virtual' block");
				break;
			}

			auto st = parse_stmt();
			if (st)
				body.push_back(std::move(st));
			else
				recover_to_newline();
		}

		if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_virtual))
		{
			auto end_kw = consume();
			auto which = consume();
			(void)end_kw;
			(void)which;
		}

		auto s = StmtVirtual{};
		s.span = kw.span;
		s.name_expr = std::move(name_expr);
		s.body = std::move(body);

		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_postpone()
	{
		auto kw = consume();

		PostponeMode mode = PostponeMode::at_end_each_pass;
		if (accept(lex::TokenKind::bang))
			mode = PostponeMode::after_stable;

		skip_newlines();

		std::vector<StmtPtr> body{};

		for (;;)
		{
			skip_newlines();

			if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_postpone))
				break;

			if (is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'postpone' block");
				break;
			}

			auto st = parse_stmt();
			if (st)
				body.push_back(std::move(st));
			else
				recover_to_newline();
		}

		if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_postpone))
		{
			auto end_kw = consume();
			auto which = consume();
			(void)end_kw;
			(void)which;
		}

		auto s = StmtPostpone{};
		s.span = kw.span;
		s.mode = mode;
		s.body = std::move(body);

		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_unexpected_end()
	{
		auto kw = consume();
		add_error(kw.span, "unexpected 'end' outside of a block");

		auto s = StmtEnd{};
		s.span = kw.span;
		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	std::optional<BinaryOp> Parser::token_to_binary_op(lex::TokenKind k) const noexcept
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

	std::size_t Parser::precedence(BinaryOp op) const noexcept
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

	bool Parser::is_right_associative(BinaryOp) const noexcept
	{
		return false;
	}

	Expr Parser::parse_expr(std::size_t min_prec)
	{
		auto lhs = parse_unary();

		for (;;)
		{
			if (is(lex::TokenKind::newline) || is(lex::TokenKind::comma)
				|| is(lex::TokenKind::rparen) || is(lex::TokenKind::eof)
				|| is(lex::TokenKind::kw_end))
				break;

			auto op_maybe = token_to_binary_op(cur().kind);
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

	Expr Parser::parse_unary()
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
			auto at_span = cur().span;

			if (accept(lex::TokenKind::dollar))
			{
				auto s = merge_spans(at_span, cur().span);
				return make_builtin(s, BuiltinKind::stream_offset);
			}

			auto rhs = parse_unary();
			auto s = merge_spans(at_span, rhs.span);
			return make_unary(s, UnaryOp::at, std::move(rhs));
		}

		return parse_primary();
	}

	Expr Parser::parse_primary()
	{
		if (is(lex::TokenKind::integer))
		{
			auto t = consume();
			return make_int(t);
		}

		if (is(lex::TokenKind::string))
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
			auto s = cur().span;
			return make_builtin(s, BuiltinKind::dollar_address);
		}

		if (accept(lex::TokenKind::lparen))
		{
			auto e = parse_expr();
			if (!expect(lex::TokenKind::rparen, "expected ')'"))
				return e;
			return e;
		}

		add_error(cur().span, "expected expression");
		auto bad = cur();
		advance();
		return Expr(bad.span, ExprInt{0});
	}

	Expr Parser::make_ident(lex::Token const& t) const
	{
		return Expr(t.span, ExprIdent{std::string(t.lexeme)});
	}

	Expr Parser::make_int(lex::Token const& t) const
	{
		if (t.integer.overflow)
		{
			auto msg = std::string("integer literal overflow: ") + std::string(t.lexeme);
			const_cast<Parser*>(this)->add_error(t.span, std::move(msg));
		}

		auto v = static_cast<std::int64_t>(t.integer.value);
		return Expr(t.span, ExprInt{v});
	}

	Expr Parser::make_str(lex::Token const& t) const
	{
		return Expr(t.span, ExprStr{unquote(t.lexeme)});
	}

	Expr Parser::make_builtin(SourceSpan s, BuiltinKind k) const
	{
		return Expr(s, ExprBuiltin{k});
	}

	Expr Parser::make_unary(SourceSpan s, UnaryOp op, Expr rhs) const
	{
		ExprUnary u{};
		u.op = op;
		u.rhs = std::make_unique<Expr>(std::move(rhs));
		return Expr(s, std::move(u));
	}

	Expr Parser::make_binary(SourceSpan s, BinaryOp op, Expr lhs, Expr rhs) const
	{
		ExprBinary b{};
		b.op = op;
		b.lhs = std::make_unique<Expr>(std::move(lhs));
		b.rhs = std::make_unique<Expr>(std::move(rhs));
		return Expr(s, std::move(b));
	}

	SourceSpan Parser::merge_spans(SourceSpan a, SourceSpan b) noexcept
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

	std::string Parser::unquote(std::string_view s)
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

} // namespace yasme::ir
