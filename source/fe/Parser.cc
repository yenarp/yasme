#include <cstddef>
#include <utility>
#include <yasme/fe/Parser.hh>
#include <yasme/macro/TokenBuffer.hh>
#include <yasme/macro/TokenSlice.hh>
#include <yasme/support/Ctype.hh>

namespace yasme::fe
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

	static bool is_ident_named(lex::Token const& tok, std::string_view value) noexcept
	{
		return tok.is(lex::TokenKind::identifier) && ieq(tok.lexeme, value);
	}

	Parser::Parser(SourceManager const& sources,
				   FileId file,
				   lex::LexerOptions lex_opt,
				   ParserOptions opt)
		: m_opt(opt)
	{
		macro::TokenBuffer buffer(sources, file, lex_opt);

		auto owned_tokens = std::make_shared<std::vector<lex::Token>>();
		auto span = buffer.tokens();
		owned_tokens->assign(span.begin(), span.end());
		m_tokens = std::move(owned_tokens);

		for (auto const& e : buffer.errors())
			m_errors.push_back(ParseError{e.span, e.message});
	}

	ParserResult Parser::parse_program()
	{
		Program p{};
		p.tokens = m_tokens;

		skip_newlines();

		while (!at_end())
		{
			auto st = parse_stmt(false);
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

	lex::Token const& Parser::cur() const noexcept
	{
		if (!m_tokens || m_tokens->empty())
		{
			static lex::Token empty{};
			return empty;
		}

		if (m_index >= m_tokens->size())
			return m_tokens->back();

		return (*m_tokens)[m_index];
	}

	lex::Token const& Parser::next() const noexcept
	{
		if (!m_tokens || m_tokens->empty())
		{
			static lex::Token empty{};
			return empty;
		}

		auto next_index = m_index + 1;
		if (next_index >= m_tokens->size())
			return m_tokens->back();

		return (*m_tokens)[next_index];
	}

	void Parser::advance() noexcept
	{
		if (m_index < m_tokens->size())
			++m_index;
	}

	bool Parser::accept(lex::TokenKind k) noexcept
	{
		if (!cur().is(k))
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
		if (cur().is(k))
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

	void Parser::recover_to_newline() noexcept
	{
		while (!at_end() && !cur().is(lex::TokenKind::newline))
			advance();

		skip_newlines();
	}

	void Parser::add_error(SourceSpan span, std::string message)
	{
		m_errors.push_back(ParseError{span, std::move(message)});
	}

	macro::TokenSlice Parser::slice_from_indices(std::size_t begin, std::size_t end) const noexcept
	{
		if (!m_tokens || m_tokens->empty() || begin > end)
			return {};

		auto* data = m_tokens->data();
		auto* b = data + begin;
		auto* e = data + end;
		return macro::make_token_slice(b, e);
	}

	SourceSpan Parser::merge_spans(SourceSpan a, SourceSpan b) const noexcept
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

	StmtPtr Parser::parse_stmt(bool in_macro)
	{
		if (cur().is(lex::TokenKind::newline))
		{
			skip_newlines();
			return nullptr;
		}

		if (cur().is(lex::TokenKind::kw_macro))
		{
			if (in_macro)
			{
				add_error(cur().span, "macro definitions cannot be nested");
				return nullptr;
			}
			return parse_stmt_macro_def();
		}

		if (cur().is(lex::TokenKind::kw_local))
		{
			if (!in_macro)
			{
				add_error(cur().span, "'local' is only valid inside macro bodies");
				return nullptr;
			}
			return parse_stmt_local();
		}

		if (cur().is(lex::TokenKind::kw_eval))
		{
			if (!in_macro)
			{
				add_error(cur().span, "'eval' is only valid inside macro bodies");
				return nullptr;
			}
			return parse_stmt_eval();
		}

		if (cur().is(lex::TokenKind::kw_match))
		{
			if (!in_macro)
			{
				add_error(cur().span, "'match' is only valid inside macro bodies");
				return nullptr;
			}
			return parse_stmt_match();
		}

		if (cur().is(lex::TokenKind::kw_org))
			return parse_stmt_org();

		if (cur().is(lex::TokenKind::identifier))
		{
			if (next().is(lex::TokenKind::colon) || next().is(lex::TokenKind::eq))
				return parse_stmt_label_or_assign();

			auto name_tok = consume();
			return parse_stmt_macro_call(std::string(name_tok.lexeme), name_tok.span);
		}

		if (cur().is(lex::TokenKind::kw_define))
			return parse_stmt_define();

		if (cur().is(lex::TokenKind::kw_load))
			return parse_stmt_load();

		if (cur().is(lex::TokenKind::kw_db) || cur().is(lex::TokenKind::kw_dw)
			|| cur().is(lex::TokenKind::kw_dd) || cur().is(lex::TokenKind::kw_dq))
		{
			auto d = cur().kind;
			return parse_stmt_emit_data(d);
		}

		if (cur().is(lex::TokenKind::kw_virtual))
			return parse_stmt_virtual(in_macro);

		if (cur().is(lex::TokenKind::kw_postpone))
			return parse_stmt_postpone(in_macro);

		if (cur().is(lex::TokenKind::kw_end))
			return parse_stmt_unexpected_end();

		if (cur().is(lex::TokenKind::eof))
			return nullptr;

		add_error(cur().span, "unexpected token at start of statement");
		advance();
		return nullptr;
	}

	StmtPtr Parser::parse_stmt_macro_def()
	{
		auto kw = consume();

		if (!cur().is(lex::TokenKind::identifier))
		{
			add_error(cur().span, "expected macro name after 'macro'");
			return nullptr;
		}

		auto name_tok = consume();
		auto name = std::string(name_tok.lexeme);

		std::vector<MacroParam> params{};
		bool saw_tokens = false;

		while (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
		{
			if (accept(lex::TokenKind::comma))
			{
				if (cur().is(lex::TokenKind::newline) || cur().is(lex::TokenKind::eof))
				{
					if (!m_opt.allow_trailing_commas)
						add_error(cur().span, "trailing comma not allowed in macro parameters");
					break;
				}
			}

			if (!cur().is(lex::TokenKind::identifier))
			{
				add_error(cur().span, "expected parameter name");
				recover_to_newline();
				break;
			}

			auto param_tok = consume();
			auto kind = MacroParamKind::plain;
			lex::Token name_part = param_tok;

			if (is_ident_named(param_tok, "name") || is_ident_named(param_tok, "ref")
				|| is_ident_named(param_tok, "tokens"))
			{
				if (!cur().is(lex::TokenKind::identifier))
				{
					add_error(cur().span, "expected parameter name after modifier");
					recover_to_newline();
					break;
				}

				if (is_ident_named(param_tok, "name"))
					kind = MacroParamKind::name;
				else if (is_ident_named(param_tok, "ref"))
					kind = MacroParamKind::ref;
				else
					kind = MacroParamKind::tokens;

				name_part = consume();
			}

			if (kind == MacroParamKind::tokens)
			{
				if (saw_tokens)
					add_error(param_tok.span, "only one 'tokens' parameter is allowed");
				saw_tokens = true;

				if (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
				{
					add_error(cur().span, "'tokens' parameter must be last in the signature");
					recover_to_newline();
					break;
				}
			}

			MacroParam param{};
			param.span = name_part.span;
			param.kind = kind;
			param.name = std::string(name_part.lexeme);
			params.push_back(std::move(param));

			if (!accept(lex::TokenKind::comma))
				break;
		}

		skip_newlines();

		std::vector<StmtPtr> body{};

		for (;;)
		{
			skip_newlines();

			if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_macro))
				break;

			if (cur().is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'macro' block");
				break;
			}

			auto st = parse_stmt(true);
			if (st)
				body.push_back(std::move(st));
			else
				recover_to_newline();
		}

		SourceSpan end_span = kw.span;
		if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_macro))
		{
			auto end_kw = consume();
			auto which = consume();
			end_span = merge_spans(end_kw.span, which.span);
		}

		StmtMacroDef def{};
		def.span = merge_spans(kw.span, end_span);
		def.name = std::move(name);
		def.params = std::move(params);
		def.body = std::move(body);

		return std::make_unique<Stmt>(Stmt(std::move(def)));
	}

	StmtPtr Parser::parse_stmt_macro_call(std::string name, SourceSpan name_span)
	{
		std::vector<macro::TokenSlice> args{};

		if (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
		{
			std::size_t arg_start = m_index;
			int paren = 0;
			int bracket = 0;
			int brace = 0;
			bool saw_token = false;

			while (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
			{
				auto tok = cur();

				if (tok.is(lex::TokenKind::lparen))
					++paren;
				else if (tok.is(lex::TokenKind::rparen) && paren > 0)
					--paren;
				else if (tok.is(lex::TokenKind::lbracket))
					++bracket;
				else if (tok.is(lex::TokenKind::rbracket) && bracket > 0)
					--bracket;
				else if (tok.is(lex::TokenKind::lbrace))
					++brace;
				else if (tok.is(lex::TokenKind::rbrace) && brace > 0)
					--brace;

				if (tok.is(lex::TokenKind::comma) && paren == 0 && bracket == 0 && brace == 0)
				{
					if (arg_start == m_index)
						add_error(tok.span, "empty macro argument");
					else
						args.push_back(slice_from_indices(arg_start, m_index));

					advance();
					arg_start = m_index;
					saw_token = false;
					continue;
				}

				saw_token = true;
				advance();
			}

			if (arg_start != m_index || saw_token)
				args.push_back(slice_from_indices(arg_start, m_index));
		}

		SourceSpan span = name_span;
		if (!args.empty())
			span = merge_spans(name_span, args.back().span);

		StmtMacroCall call{};
		call.span = span;
		call.callee = std::move(name);
		call.args = std::move(args);
		return std::make_unique<Stmt>(Stmt(std::move(call)));
	}

	StmtPtr Parser::parse_stmt_local()
	{
		auto kw = consume();

		std::vector<std::string> names{};
		SourceSpan last_span = kw.span;

		for (;;)
		{
			if (!cur().is(lex::TokenKind::identifier))
			{
				add_error(cur().span, "expected identifier after 'local'");
				break;
			}

			auto name_tok = consume();
			names.push_back(std::string(name_tok.lexeme));
			last_span = name_tok.span;

			if (!accept(lex::TokenKind::comma))
				break;

			if (cur().is(lex::TokenKind::newline) || cur().is(lex::TokenKind::eof))
			{
				if (!m_opt.allow_trailing_commas)
					add_error(cur().span, "trailing comma not allowed in 'local'");
				break;
			}
		}

		StmtLocal st{};
		st.span = merge_spans(kw.span, last_span);
		st.names = std::move(names);
		return std::make_unique<Stmt>(Stmt(std::move(st)));
	}

	StmtPtr Parser::parse_stmt_eval()
	{
		auto kw = consume();

		if (!cur().is(lex::TokenKind::identifier))
		{
			add_error(cur().span, "expected output identifier after 'eval'");
			return nullptr;
		}

		auto out_tok = consume();

		if (!expect(lex::TokenKind::comma, "expected ',' after eval output"))
			return nullptr;

		if (!cur().is(lex::TokenKind::identifier))
		{
			add_error(cur().span, "expected tokens parameter name after ','");
			return nullptr;
		}

		auto tokens_tok = consume();

		StmtEval st{};
		st.span = merge_spans(kw.span, tokens_tok.span);
		st.out_name = std::string(out_tok.lexeme);
		st.tokens_name = std::string(tokens_tok.lexeme);
		return std::make_unique<Stmt>(Stmt(std::move(st)));
	}

	StmtPtr Parser::parse_stmt_match()
	{
		auto kw = consume();

		if (!cur().is(lex::TokenKind::identifier))
		{
			add_error(cur().span, "expected tokens parameter name after 'match'");
			return nullptr;
		}

		auto name_tok = consume();

		if (!expect(lex::TokenKind::comma, "expected ',' after match parameter"))
			return nullptr;

		auto pattern_start = m_index;
		while (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
			advance();

		if (pattern_start == m_index)
			add_error(cur().span, "expected match pattern");

		auto pattern = slice_from_indices(pattern_start, m_index);

		skip_newlines();

		std::vector<StmtPtr> on_match{};
		std::vector<StmtPtr> on_else{};
		bool has_else = false;

		for (;;)
		{
			skip_newlines();

			if (cur().is(lex::TokenKind::kw_else))
				break;

			if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_match))
				break;

			if (cur().is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'match' block");
				break;
			}

			auto st = parse_stmt(true);
			if (st)
				on_match.push_back(std::move(st));
			else
				recover_to_newline();
		}

		if (cur().is(lex::TokenKind::kw_else))
		{
			has_else = true;
			advance();
			if (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
				recover_to_newline();

			skip_newlines();

			for (;;)
			{
				skip_newlines();

				if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_match))
					break;

				if (cur().is(lex::TokenKind::eof))
				{
					add_error(cur().span, "unexpected end of file in 'match' block");
					break;
				}

				auto st = parse_stmt(true);
				if (st)
					on_else.push_back(std::move(st));
				else
					recover_to_newline();
			}
		}

		SourceSpan end_span = kw.span;
		if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_match))
		{
			auto end_kw = consume();
			auto which = consume();
			end_span = merge_spans(end_kw.span, which.span);
		}

		StmtMatch st{};
		st.span = merge_spans(kw.span, end_span);
		st.tokens_name = std::string(name_tok.lexeme);
		st.pattern = pattern;
		st.on_match = std::move(on_match);
		st.on_else = std::move(on_else);
		st.has_else = has_else;

		return std::make_unique<Stmt>(Stmt(std::move(st)));
	}

	StmtPtr Parser::parse_stmt_org()
	{
		auto kw = consume();
		auto addr = parse_expr();

		ir::StmtOrg s{};
		s.span = kw.span;
		s.address = std::move(addr);

		return std::make_unique<Stmt>(
			Stmt(StmtNormal{std::make_unique<ir::Stmt>(ir::Stmt(std::move(s)))}));
	}

	StmtPtr Parser::parse_stmt_label_or_assign()
	{
		auto name_tok = consume();
		auto name = std::string(name_tok.lexeme);

		if (accept(lex::TokenKind::colon))
		{
			ir::StmtLabel s{};
			s.span = merge_spans(name_tok.span, name_tok.span);
			s.name = std::move(name);
			return std::make_unique<Stmt>(
				Stmt(StmtNormal{std::make_unique<ir::Stmt>(ir::Stmt(std::move(s)))}));
		}

		if (!accept(lex::TokenKind::eq))
		{
			add_error(cur().span, "expected ':' for label or '=' for assignment");
			return nullptr;
		}

		auto rhs = parse_expr();

		ir::StmtAssign s{};
		s.span = merge_spans(name_tok.span, rhs.span);
		s.name = std::move(name);
		s.rhs = std::move(rhs);

		return std::make_unique<Stmt>(
			Stmt(StmtNormal{std::make_unique<ir::Stmt>(ir::Stmt(std::move(s)))}));
	}

	StmtPtr Parser::parse_stmt_define()
	{
		auto kw = consume();

		if (!cur().is(lex::TokenKind::identifier))
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

		ir::StmtDefine s{};
		s.span = merge_spans(kw.span, value.span);
		s.name = std::move(name);
		s.value = std::move(value);

		return std::make_unique<Stmt>(
			Stmt(StmtNormal{std::make_unique<ir::Stmt>(ir::Stmt(std::move(s)))}));
	}

	StmtPtr Parser::parse_stmt_emit_data(lex::TokenKind directive)
	{
		auto kw = consume();

		ir::DataUnit unit = ir::DataUnit::u8;
		switch (directive)
		{
			case lex::TokenKind::kw_db:
				unit = ir::DataUnit::u8;
				break;
			case lex::TokenKind::kw_dw:
				unit = ir::DataUnit::u16;
				break;
			case lex::TokenKind::kw_dd:
				unit = ir::DataUnit::u32;
				break;
			case lex::TokenKind::kw_dq:
				unit = ir::DataUnit::u64;
				break;
			default:
				break;
		}

		std::vector<ir::Expr> items{};

		if (cur().is(lex::TokenKind::newline) || cur().is(lex::TokenKind::eof))
		{
			if (!m_opt.allow_empty_data_list)
				add_error(cur().span, "expected at least one item for data directive");
		}
		else
		{
			items.push_back(parse_expr());

			while (accept(lex::TokenKind::comma))
			{
				if (cur().is(lex::TokenKind::newline) || cur().is(lex::TokenKind::eof))
				{
					if (!m_opt.allow_trailing_commas)
						add_error(cur().span, "trailing comma not allowed here");
					break;
				}

				items.push_back(parse_expr());
			}
		}

		ir::StmtEmitData s{};
		s.span = kw.span;
		s.unit = unit;
		s.items = std::move(items);

		return std::make_unique<Stmt>(
			Stmt(StmtNormal{std::make_unique<ir::Stmt>(ir::Stmt(std::move(s)))}));
	}

	StmtPtr Parser::parse_stmt_load()
	{
		auto kw = consume();

		if (!(cur().is(lex::TokenKind::kw_db) || cur().is(lex::TokenKind::kw_dw)
			  || cur().is(lex::TokenKind::kw_dd) || cur().is(lex::TokenKind::kw_dq)))
		{
			add_error(cur().span, "expected data unit after 'load'");
			return nullptr;
		}

		auto unit_tok = consume();
		ir::DataUnit unit = ir::DataUnit::u8;
		switch (unit_tok.kind)
		{
			case lex::TokenKind::kw_db:
				unit = ir::DataUnit::u8;
				break;
			case lex::TokenKind::kw_dw:
				unit = ir::DataUnit::u16;
				break;
			case lex::TokenKind::kw_dd:
				unit = ir::DataUnit::u32;
				break;
			case lex::TokenKind::kw_dq:
				unit = ir::DataUnit::u64;
				break;
			default:
				break;
		}

		if (!cur().is(lex::TokenKind::identifier))
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

		ir::StmtLoad s{};
		s.span = merge_spans(kw.span, offset.span);
		s.unit = unit;
		s.dest = std::string(dest_tok.lexeme);
		s.stream = std::move(stream);
		s.offset = std::move(offset);

		return std::make_unique<Stmt>(
			Stmt(StmtNormal{std::make_unique<ir::Stmt>(ir::Stmt(std::move(s)))}));
	}

	StmtPtr Parser::parse_stmt_virtual(bool in_macro)
	{
		auto kw = consume();

		std::optional<ir::Expr> name_expr{};

		if (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
			name_expr = parse_expr();

		skip_newlines();

		std::vector<StmtPtr> body{};

		for (;;)
		{
			skip_newlines();

			if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_virtual))
				break;

			if (cur().is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'virtual' block");
				break;
			}

			auto st = parse_stmt(in_macro);
			if (st)
				body.push_back(std::move(st));
			else
				recover_to_newline();
		}

		if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_virtual))
		{
			auto end_kw = consume();
			auto which = consume();
			(void)end_kw;
			(void)which;
		}

		StmtVirtual s{};
		s.span = kw.span;
		s.name_expr = std::move(name_expr);
		s.body = std::move(body);

		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_postpone(bool in_macro)
	{
		auto kw = consume();

		ir::PostponeMode mode = ir::PostponeMode::at_end_each_pass;
		if (accept(lex::TokenKind::bang))
			mode = ir::PostponeMode::after_stable;

		skip_newlines();

		std::vector<StmtPtr> body{};

		for (;;)
		{
			skip_newlines();

			if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_postpone))
				break;

			if (cur().is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'postpone' block");
				break;
			}

			auto st = parse_stmt(in_macro);
			if (st)
				body.push_back(std::move(st));
			else
				recover_to_newline();
		}

		if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_postpone))
		{
			auto end_kw = consume();
			auto which = consume();
			(void)end_kw;
			(void)which;
		}

		StmtPostpone s{};
		s.span = kw.span;
		s.mode = mode;
		s.body = std::move(body);

		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_unexpected_end()
	{
		auto kw = consume();
		add_error(kw.span, "unexpected 'end' outside of a block");

		ir::StmtEnd s{};
		s.span = kw.span;
		return std::make_unique<Stmt>(
			Stmt(StmtNormal{std::make_unique<ir::Stmt>(ir::Stmt(std::move(s)))}));
	}

	ir::Expr Parser::parse_expr(std::size_t min_prec)
	{
		auto cur_fn = [this]() -> lex::Token const& { return this->cur(); };
		auto advance_fn = [this]() { this->advance(); };
		auto error_fn = [this](SourceSpan span, std::string message) {
			this->add_error(span, std::move(message));
		};

		ir::ExprParser parser(cur_fn, advance_fn, error_fn);
		return parser.parse_expr(min_prec);
	}

} // namespace yasme::fe
