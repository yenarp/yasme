#include <utility>
#include <yasme/ir/ExprParser.hh>
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

		if (is(lex::TokenKind::kw_if))
			return parse_stmt_if();
		if (is(lex::TokenKind::kw_repeat))
			return parse_stmt_repeat();
		if (is(lex::TokenKind::kw_while))
			return parse_stmt_while();
		if (is(lex::TokenKind::kw_for))
			return parse_stmt_for();
		if (is(lex::TokenKind::kw_break))
			return parse_stmt_break();
		if (is(lex::TokenKind::kw_continue))
			return parse_stmt_continue();

		if (is(lex::TokenKind::kw_org))
			return parse_stmt_org();

		if (is(lex::TokenKind::identifier))
			return parse_stmt_label_or_assign();

		if (is(lex::TokenKind::kw_define))
			return parse_stmt_define();

		if (is(lex::TokenKind::kw_load))
			return parse_stmt_load();

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

	StmtPtr Parser::parse_stmt_load()
	{
		auto kw = consume();

		if (!(is(lex::TokenKind::kw_db) || is(lex::TokenKind::kw_dw) || is(lex::TokenKind::kw_dd)
			  || is(lex::TokenKind::kw_dq)))
		{
			add_error(cur().span, "expected data unit after 'load'");
			return nullptr;
		}

		auto unit_tok = consume();
		DataUnit unit = DataUnit::u8;
		switch (unit_tok.kind)
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

		if (!is(lex::TokenKind::identifier))
		{
			add_error(cur().span, "expected destination identifier after load unit");
			return nullptr;
		}

		auto dest_tok = consume();

		if (!expect(lex::TokenKind::comma, "expected ',' after load destination"))
			return nullptr;

		auto stream = parse_expr();

		if (!expect(lex::TokenKind::comma, "expected ',' after load stream"))
			return nullptr;

		auto offset = parse_expr();

		StmtLoad s{};
		s.span = merge_spans(kw.span, offset.span);
		s.unit = unit;
		s.dest = std::string(dest_tok.lexeme);
		s.stream = std::move(stream);
		s.offset = std::move(offset);

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
			consume();
			consume();
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
			consume();
			consume();
		}

		auto s = StmtPostpone{};
		s.span = kw.span;
		s.mode = mode;
		s.body = std::move(body);

		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_if()
	{
		auto kw = consume();

		auto cond = parse_expr();
		skip_newlines();

		std::vector<StmtPtr> then_body{};
		std::vector<StmtPtr> else_body{};
		bool has_else = false;

		for (;;)
		{
			skip_newlines();

			if (is(lex::TokenKind::kw_else))
				break;
			if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_if))
				break;

			if (is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'if' block");
				break;
			}

			auto st = parse_stmt();
			if (st)
				then_body.push_back(std::move(st));
			else
				recover_to_newline();
		}

		if (is(lex::TokenKind::kw_else))
		{
			consume();
			has_else = true;
			skip_newlines();

			for (;;)
			{
				skip_newlines();

				if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_if))
					break;

				if (is(lex::TokenKind::eof))
				{
					add_error(cur().span, "unexpected end of file in 'if' block");
					break;
				}

				auto st = parse_stmt();
				if (st)
					else_body.push_back(std::move(st));
				else
					recover_to_newline();
			}
		}

		if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_if))
		{
			consume();
			consume();
		}

		StmtIf s{};
		s.span = kw.span;
		s.cond = std::move(cond);
		s.then_body = std::move(then_body);
		s.else_body = std::move(else_body);
		s.has_else = has_else;
		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_repeat()
	{
		auto kw = consume();

		auto count = parse_expr();
		skip_newlines();

		std::vector<StmtPtr> body{};
		for (;;)
		{
			skip_newlines();

			if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_repeat))
				break;

			if (is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'repeat' block");
				break;
			}

			auto st = parse_stmt();
			if (st)
				body.push_back(std::move(st));
			else
				recover_to_newline();
		}

		if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_repeat))
		{
			consume();
			consume();
		}

		StmtRepeat s{};
		s.span = kw.span;
		s.count = std::move(count);
		s.body = std::move(body);
		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_while()
	{
		auto kw = consume();

		auto cond = parse_expr();
		skip_newlines();

		std::vector<StmtPtr> body{};
		for (;;)
		{
			skip_newlines();

			if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_while))
				break;

			if (is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'while' block");
				break;
			}

			auto st = parse_stmt();
			if (st)
				body.push_back(std::move(st));
			else
				recover_to_newline();
		}

		if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_while))
		{
			consume();
			consume();
		}

		StmtWhile s{};
		s.span = kw.span;
		s.cond = std::move(cond);
		s.body = std::move(body);
		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_for()
	{
		auto kw = consume();

		if (!is(lex::TokenKind::identifier))
		{
			add_error(cur().span, "expected loop variable after 'for'");
			recover_to_newline();
			return nullptr;
		}

		auto var_tok = consume();
		std::string var = std::string(var_tok.lexeme);

		if (accept(lex::TokenKind::kw_in))
		{
			auto str = parse_expr();
			skip_newlines();

			std::vector<StmtPtr> body{};
			for (;;)
			{
				skip_newlines();

				if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_for))
					break;

				if (is(lex::TokenKind::eof))
				{
					add_error(cur().span, "unexpected end of file in 'for' block");
					break;
				}

				auto st = parse_stmt();
				if (st)
					body.push_back(std::move(st));
				else
					recover_to_newline();
			}

			if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_for))
			{
				consume();
				consume();
			}

			StmtForChars s{};
			s.span = kw.span;
			s.var = std::move(var);
			s.str = std::move(str);
			s.body = std::move(body);
			return std::make_unique<Stmt>(Stmt(std::move(s)));
		}

		if (!accept(lex::TokenKind::eq))
		{
			add_error(cur().span, "expected '=' or 'in' after loop variable");
			recover_to_newline();
			return nullptr;
		}

		auto start = parse_expr();
		if (!expect(lex::TokenKind::comma, "expected ',' after for start expression"))
		{
			recover_to_newline();
			return nullptr;
		}

		auto end = parse_expr();

		std::optional<Expr> step{};
		if (accept(lex::TokenKind::comma))
			step = parse_expr();

		skip_newlines();

		std::vector<StmtPtr> body{};
		for (;;)
		{
			skip_newlines();

			if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_for))
				break;

			if (is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'for' block");
				break;
			}

			auto st = parse_stmt();
			if (st)
				body.push_back(std::move(st));
			else
				recover_to_newline();
		}

		if (is(lex::TokenKind::kw_end) && is_next(lex::TokenKind::kw_for))
		{
			consume();
			consume();
		}

		StmtForNumeric s{};
		s.span = kw.span;
		s.var = std::move(var);
		s.start = std::move(start);
		s.end = std::move(end);
		s.step = std::move(step);
		s.body = std::move(body);
		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_break()
	{
		auto kw = consume();
		StmtBreak s{};
		s.span = kw.span;
		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_continue()
	{
		auto kw = consume();
		StmtContinue s{};
		s.span = kw.span;
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

	Expr Parser::parse_expr(std::size_t min_prec)
	{
		auto cur = [this]() -> lex::Token const& { return this->cur(); };
		auto advance = [this]() { this->advance(); };
		auto error = [this](SourceSpan span, std::string message) {
			this->add_error(span, std::move(message));
		};

		ExprParser parser(cur, advance, error);
		return parser.parse_expr(min_prec);
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

} // namespace yasme::ir
