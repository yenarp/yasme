#include <cstddef>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <yasme/fe/Parser.hh>
#include <yasme/macro/TokenBuffer.hh>
#include <yasme/macro/TokenSlice.hh>
#include <yasme/support/Ctype.hh>

namespace yasme::fe
{
	namespace fs = std::filesystem;

	[[nodiscard]] static std::string unquote_stringlike(std::string_view s)
	{
		if (s.size() < 2)
			return std::string(s);

		auto const q0 = s.front();
		auto const q1 = s.back();
		if ((q0 == '"' && q1 == '"') || (q0 == '\'' && q1 == '\''))
			return std::string(s.substr(1, s.size() - 2));

		return std::string(s);
	}

	[[nodiscard]] static bool is_stringlike(lex::TokenKind k) noexcept
	{
		return k == lex::TokenKind::string || k == lex::TokenKind::char_literal;
	}

	[[nodiscard]] static std::string normalize_path(fs::path p)
	{
		std::error_code ec{};
		auto const canon = fs::weakly_canonical(p, ec);
		if (!ec)
			return canon.generic_string();

		return p.lexically_normal().generic_string();
	}

	static bool ieq(std::string_view a, std::string_view b) noexcept
	{
		if (a.size() != b.size())
			return false;

		for (std::size_t i = 0; i < a.size(); ++i)
			if (ascii_tolower(a[i]) != ascii_tolower(b[i]))
				return false;

		return true;
	}

	static bool is_ident_named(lex::Token const& tok, std::string_view value) noexcept
	{
		return tok.is(lex::TokenKind::identifier) && ieq(tok.lexeme, value);
	}

	Parser::Parser(SourceManager& sources,
				   FileId file,
				   lex::LexerOptions lex_opt,
				   ParserOptions opt)
		: m_sources(std::addressof(sources)), m_file(file), m_lex_opt(lex_opt),
		  m_opt(std::move(opt))
	{
		m_tokens = std::make_shared<std::vector<lex::Token>>();

		macro::TokenBuffer buf(sources, file, lex_opt);
		for (auto const& le : buf.errors())
			m_errors.push_back(ParseError{le.span, le.message});

		auto tokens = buf.tokens();
		for (auto token : tokens)
			m_tokens->push_back(token);

		m_index = 0;
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

		if (cur().is(lex::TokenKind::kw_if))
			return parse_stmt_if(in_macro);
		if (cur().is(lex::TokenKind::kw_repeat))
			return parse_stmt_repeat(in_macro);
		if (cur().is(lex::TokenKind::kw_while))
			return parse_stmt_while(in_macro);
		if (cur().is(lex::TokenKind::kw_for))
			return parse_stmt_for(in_macro);
		if (cur().is(lex::TokenKind::kw_break))
			return parse_stmt_break();
		if (cur().is(lex::TokenKind::kw_continue))
			return parse_stmt_continue();

		if (cur().is(lex::TokenKind::kw_macro))
			return parse_stmt_macro_def();

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

		if (cur().is(lex::TokenKind::kw_include))
			return parse_stmt_include();

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

		if (cur().is(lex::TokenKind::kw_error))
		{
			if (!in_macro)
			{
				add_error(cur().span, "'error' is only valid inside macro bodies");
				return nullptr;
			}
			return parse_stmt_macro_error();
		}

		add_error(cur().span, "unexpected token at start of statement");
		advance();
		return nullptr;
	}

	StmtPtr Parser::parse_stmt_include()
	{
		auto kw = consume();

		if (!is_stringlike(cur().kind))
		{
			add_error(cur().span, "expected a quoted path after include");
			recover_to_newline();
			return nullptr;
		}

		auto path_tok = consume();
		auto raw = unquote_stringlike(path_tok.lexeme);

		auto from_name = std::string(m_sources->name(m_file));
		fs::path from_path(from_name);
		fs::path raw_path(raw);

		std::vector<fs::path> candidates{};
		if (raw_path.is_absolute())
		{
			candidates.push_back(raw_path);
		}
		else
		{
			auto parent = from_path.parent_path();
			if (!parent.empty())
				candidates.push_back(parent / raw_path);

			for (auto const& inc : m_opt.include_paths)
				candidates.push_back(fs::path(inc) / raw_path);

			candidates.push_back(raw_path);
		}

		FileId inc_file{};
		std::string inc_key{};
		std::string last_err{};

		for (auto const& cand : candidates)
		{
			auto key = normalize_path(cand);

			if (auto it = m_include_cache.find(key); it != m_include_cache.end())
			{
				inc_file = it->second;
				inc_key = std::move(key);
				last_err.clear();
				break;
			}

			auto res = m_sources->open_read(cand.string());
			if (res.ok())
			{
				inc_file = res.value();
				inc_key = std::move(key);
				m_include_cache.emplace(inc_key, inc_file);
				last_err.clear();
				break;
			}

			last_err = res.err();
		}

		if (inc_key.empty())
			add_error(path_tok.span, "cannot open include file '" + raw + "': " + last_err);

		SourceSpan last_span = path_tok.span;
		while (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
		{
			add_error(cur().span, "unexpected token after include path");
			last_span = cur().span;
			advance();
		}

		StmtInclude st{};
		st.span = merge_spans(kw.span, last_span);
		st.raw_path = std::move(raw);
		st.file = inc_file;
		st.normalized_key = std::move(inc_key);
		return std::make_unique<Stmt>(Stmt(std::move(st)));
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

			if (!(cur().is(lex::TokenKind::identifier) || cur().is(lex::TokenKind::kw_ref)))
			{
				add_error(cur().span, "expected parameter name");
				recover_to_newline();
				break;
			}

			auto param_tok = consume();
			auto kind = MacroParamKind::plain;
			lex::Token name_part = param_tok;

			auto const is_name_mod = is_ident_named(param_tok, "name");
			auto const is_tokens_mod = is_ident_named(param_tok, "tokens");
			auto const is_ref_mod =
				param_tok.is(lex::TokenKind::kw_ref) || is_ident_named(param_tok, "ref");

			if (is_name_mod || is_ref_mod || is_tokens_mod)
			{
				if (!cur().is(lex::TokenKind::identifier))
				{
					add_error(cur().span, "expected parameter name after modifier");
					recover_to_newline();
					break;
				}

				if (is_name_mod)
					kind = MacroParamKind::name;
				else if (is_ref_mod)
					kind = MacroParamKind::ref;
				else
					kind = MacroParamKind::tokens;

				name_part = consume();
			}
			else if (!param_tok.is(lex::TokenKind::identifier))
			{
				add_error(param_tok.span, "expected parameter name");
				recover_to_newline();
				break;
			}

			if (kind == MacroParamKind::tokens)
			{
				if (saw_tokens)
					add_error(param_tok.span, "only one 'tokens' parameter is allowed");
				saw_tokens = true;

				if (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
					add_error(cur().span, "'tokens' parameter must be last in the signature");
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
		macro::TokenSlice raw_args{};

		if (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
		{
			auto raw_start = m_index;

			std::size_t arg_start = m_index;
			int paren = 0;
			int bracket = 0;
			int brace = 0;

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
					continue;
				}

				advance();
			}

			if (arg_start != m_index)
				args.push_back(slice_from_indices(arg_start, m_index));

			raw_args = slice_from_indices(raw_start, m_index);
		}

		SourceSpan span = name_span;
		if (raw_args.begin && raw_args.end)
			span = merge_spans(name_span, raw_args.span);
		else if (!args.empty())
			span = merge_spans(name_span, args.back().span);

		StmtMacroCall call{};
		call.span = span;
		call.callee = std::move(name);
		call.args = std::move(args);
		call.raw_args = raw_args;
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

		bool force_pattern_eval = false;
		if (cur().is(lex::TokenKind::star))
		{
			force_pattern_eval = true;
			consume();
		}

		if (!cur().is(lex::TokenKind::identifier))
		{
			add_error(cur().span, "expected output identifier after 'eval'");
			return nullptr;
		}

		auto out_tok = consume();

		if (!expect(lex::TokenKind::comma, "expected ',' after eval output"))
			return nullptr;

		StmtEval st{};
		st.span = kw.span;
		st.out_name = std::string(out_tok.lexeme);

		if (!force_pattern_eval)
		{
			auto tokens_start = m_index;
			if (cur().is(lex::TokenKind::identifier))
			{
				auto tokens_tok = consume();
				if (cur().is(lex::TokenKind::newline) || cur().is(lex::TokenKind::eof))
				{
					st.span = merge_spans(kw.span, tokens_tok.span);
					st.input_kind = StmtEval::InputKind::tokens_name;
					st.output_kind = StmtEval::OutputKind::value;
					st.tokens_name = std::string(tokens_tok.lexeme);
					return std::make_unique<Stmt>(Stmt(std::move(st)));
				}
				m_index = tokens_start;
			}
		}

		auto pattern_start = m_index;
		while (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
			advance();

		if (pattern_start == m_index)
		{
			add_error(cur().span, "expected eval pattern after ','");
			return nullptr;
		}

		st.input_kind = StmtEval::InputKind::pattern;
		st.output_kind =
			force_pattern_eval ? StmtEval::OutputKind::value : StmtEval::OutputKind::tokens;
		st.pattern = slice_from_indices(pattern_start, m_index);
		st.span = merge_spans(kw.span, st.pattern.span);
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
		auto tokens_name = std::string(name_tok.lexeme);

		if (!expect(lex::TokenKind::comma, "expected ',' after match parameter"))
			return nullptr;

		auto read_pattern = [this]() -> macro::TokenSlice {
			auto pattern_start = m_index;
			while (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
				advance();

			if (pattern_start == m_index)
				add_error(cur().span, "expected match pattern");

			return slice_from_indices(pattern_start, m_index);
		};

		auto parse_body_until =
			[this](std::vector<StmtPtr>& dst, bool stop_on_else, lex::TokenKind end_kw_which) {
				for (;;)
				{
					skip_newlines();

					if (stop_on_else && cur().is(lex::TokenKind::kw_else))
						break;

					if (cur().is(lex::TokenKind::kw_end) && next().is(end_kw_which))
						break;

					if (cur().is(lex::TokenKind::eof))
					{
						add_error(cur().span, "unexpected end of file in block");
						break;
					}

					auto st = parse_stmt(true);
					if (st)
						dst.push_back(std::move(st));
					else
						recover_to_newline();
				}
			};

		std::vector<macro::TokenSlice> patterns{};
		std::vector<std::vector<StmtPtr>> bodies{};

		auto first_pattern = read_pattern();
		skip_newlines();

		std::vector<StmtPtr> first_body{};
		parse_body_until(first_body, true, lex::TokenKind::kw_match);

		patterns.push_back(first_pattern);
		bodies.push_back(std::move(first_body));

		bool has_final_else = false;
		std::vector<StmtPtr> final_else_body{};

		while (cur().is(lex::TokenKind::kw_else))
		{
			consume();

			if (cur().is(lex::TokenKind::kw_match))
			{
				consume();

				if (!cur().is(lex::TokenKind::identifier))
				{
					add_error(cur().span, "expected tokens parameter name after 'else match'");
					recover_to_newline();
					break;
				}

				auto chained_name_tok = consume();
				if (!ieq(chained_name_tok.lexeme, tokens_name))
					add_error(chained_name_tok.span,
							  "tokens name in 'else match' differs from initial 'match'");

				if (!expect(lex::TokenKind::comma, "expected ',' after match parameter"))
					return nullptr;

				auto pat = read_pattern();
				skip_newlines();

				std::vector<StmtPtr> body{};
				parse_body_until(body, true, lex::TokenKind::kw_match);

				patterns.push_back(pat);
				bodies.push_back(std::move(body));
				continue;
			}

			has_final_else = true;

			if (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
			{
				auto st = parse_stmt(true);
				if (st)
					final_else_body.push_back(std::move(st));
				else
					recover_to_newline();
			}

			skip_newlines();
			parse_body_until(final_else_body, false, lex::TokenKind::kw_match);
			break;
		}

		SourceSpan end_span = kw.span;
		if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_match))
		{
			auto end_kw = consume();
			auto which = consume();
			end_span = merge_spans(end_kw.span, which.span);
		}

		auto full_span = merge_spans(kw.span, end_span);

		StmtPtr nested{};
		for (std::size_t i = patterns.size(); i > 0; --i)
		{
			auto idx = i - 1;

			StmtMatch st{};
			st.span = full_span;
			st.tokens_name = tokens_name;
			st.pattern = patterns[idx];
			st.on_match = std::move(bodies[idx]);

			if (!nested)
			{
				st.has_else = has_final_else;
				st.on_else = std::move(final_else_body);
			}
			else
			{
				st.has_else = true;
				std::vector<StmtPtr> else_vec{};
				else_vec.push_back(std::move(nested));
				st.on_else = std::move(else_vec);
			}

			nested = std::make_unique<Stmt>(Stmt(std::move(st)));
		}

		return nested;
	}

	StmtPtr Parser::parse_stmt_org()
	{
		auto kw = consume();
		auto addr = parse_expr();

		ir::StmtOrg s{};
		s.span = merge_spans(kw.span, addr.span);
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
			s.span = name_tok.span;
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
		s.span = items.empty() ? kw.span : merge_spans(kw.span, items.back().span);
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

		SourceSpan end_span = kw.span;
		if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_virtual))
		{
			auto end_kw = consume();
			auto which = consume();
			end_span = merge_spans(end_kw.span, which.span);
		}

		StmtVirtual s{};
		s.span = merge_spans(kw.span, end_span);
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

		SourceSpan end_span = kw.span;
		if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_postpone))
		{
			auto end_kw = consume();
			auto which = consume();
			end_span = merge_spans(end_kw.span, which.span);
		}

		StmtPostpone s{};
		s.span = merge_spans(kw.span, end_span);
		s.mode = mode;
		s.body = std::move(body);

		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_if(bool in_macro)
	{
		auto kw = consume();

		auto parse_then_body_until = [this, in_macro](std::vector<StmtPtr>& dst,
													  bool stop_on_else) {
			for (;;)
			{
				skip_newlines();

				if (stop_on_else && cur().is(lex::TokenKind::kw_else))
					break;

				if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_if))
					break;

				if (cur().is(lex::TokenKind::eof))
				{
					add_error(cur().span, "unexpected end of file in 'if' block");
					break;
				}

				auto st = parse_stmt(in_macro);
				if (st)
					dst.push_back(std::move(st));
				else
					recover_to_newline();
			}
		};

		std::vector<ir::Expr> conds{};
		std::vector<std::vector<StmtPtr>> thens{};

		conds.push_back(parse_expr());
		skip_newlines();

		std::vector<StmtPtr> first_then{};
		parse_then_body_until(first_then, true);
		thens.push_back(std::move(first_then));

		bool has_final_else = false;
		std::vector<StmtPtr> final_else_body{};

		while (cur().is(lex::TokenKind::kw_else))
		{
			consume();

			if (cur().is(lex::TokenKind::kw_if))
			{
				consume();

				conds.push_back(parse_expr());
				skip_newlines();

				std::vector<StmtPtr> then_body{};
				parse_then_body_until(then_body, true);
				thens.push_back(std::move(then_body));
				continue;
			}

			has_final_else = true;

			if (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
			{
				auto st = parse_stmt(in_macro);
				if (st)
					final_else_body.push_back(std::move(st));
				else
					recover_to_newline();
			}

			skip_newlines();
			parse_then_body_until(final_else_body, false);
			break;
		}

		SourceSpan end_span = kw.span;
		if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_if))
		{
			auto end_kw = consume();
			auto which = consume();
			end_span = merge_spans(end_kw.span, which.span);
		}

		auto full_span = merge_spans(kw.span, end_span);

		StmtPtr nested{};
		for (std::size_t i = conds.size(); i > 0; --i)
		{
			auto idx = i - 1;

			StmtIf s{};
			s.span = full_span;
			s.cond = std::move(conds[idx]);
			s.then_body = std::move(thens[idx]);

			if (!nested)
			{
				s.has_else = has_final_else;
				s.else_body = std::move(final_else_body);
			}
			else
			{
				s.has_else = true;
				std::vector<StmtPtr> else_vec{};
				else_vec.push_back(std::move(nested));
				s.else_body = std::move(else_vec);
			}

			nested = std::make_unique<Stmt>(Stmt(std::move(s)));
		}

		return nested;
	}

	StmtPtr Parser::parse_stmt_repeat(bool in_macro)
	{
		auto kw = consume();
		auto count = parse_expr();
		skip_newlines();

		std::vector<StmtPtr> body{};
		for (;;)
		{
			skip_newlines();

			if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_repeat))
				break;

			if (cur().is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'repeat' block");
				break;
			}

			auto st = parse_stmt(in_macro);
			if (st)
				body.push_back(std::move(st));
			else
				recover_to_newline();
		}

		SourceSpan end_span = kw.span;
		if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_repeat))
		{
			auto end_kw = consume();
			auto which = consume();
			end_span = merge_spans(end_kw.span, which.span);
		}

		StmtRepeat s{};
		s.span = merge_spans(kw.span, end_span);
		s.count = std::move(count);
		s.body = std::move(body);

		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_while(bool in_macro)
	{
		auto kw = consume();
		auto cond = parse_expr();
		skip_newlines();

		std::vector<StmtPtr> body{};
		for (;;)
		{
			skip_newlines();

			if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_while))
				break;

			if (cur().is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'while' block");
				break;
			}

			auto st = parse_stmt(in_macro);
			if (st)
				body.push_back(std::move(st));
			else
				recover_to_newline();
		}

		SourceSpan end_span = kw.span;
		if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_while))
		{
			auto end_kw = consume();
			auto which = consume();
			end_span = merge_spans(end_kw.span, which.span);
		}

		StmtWhile s{};
		s.span = merge_spans(kw.span, end_span);
		s.cond = std::move(cond);
		s.body = std::move(body);

		return std::make_unique<Stmt>(Stmt(std::move(s)));
	}

	StmtPtr Parser::parse_stmt_for(bool in_macro)
	{
		auto kw = consume();

		if (!cur().is(lex::TokenKind::identifier))
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

				if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_for))
					break;

				if (cur().is(lex::TokenKind::eof))
				{
					add_error(cur().span, "unexpected end of file in 'for' block");
					break;
				}

				auto st = parse_stmt(in_macro);
				if (st)
					body.push_back(std::move(st));
				else
					recover_to_newline();
			}

			SourceSpan end_span = kw.span;
			if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_for))
			{
				auto end_kw = consume();
				auto which = consume();
				end_span = merge_spans(end_kw.span, which.span);
			}

			StmtForChars s{};
			s.span = merge_spans(kw.span, end_span);
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

		std::optional<ir::Expr> step{};
		if (accept(lex::TokenKind::comma))
		{
			if (cur().is(lex::TokenKind::newline) || cur().is(lex::TokenKind::eof))
			{
				if (!m_opt.allow_trailing_commas)
					add_error(cur().span, "trailing comma not allowed here");
			}
			else
			{
				step = parse_expr();
			}
		}

		skip_newlines();

		std::vector<StmtPtr> body{};
		for (;;)
		{
			skip_newlines();

			if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_for))
				break;

			if (cur().is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'for' block");
				break;
			}

			auto st = parse_stmt(in_macro);
			if (st)
				body.push_back(std::move(st));
			else
				recover_to_newline();
		}

		SourceSpan end_span = kw.span;
		if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_for))
		{
			auto end_kw = consume();
			auto which = consume();
			end_span = merge_spans(end_kw.span, which.span);
		}

		StmtForNumeric s{};
		s.span = merge_spans(kw.span, end_span);
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
		ir::StmtBreak s{};
		s.span = kw.span;
		return std::make_unique<Stmt>(
			Stmt(StmtNormal{std::make_unique<ir::Stmt>(ir::Stmt(std::move(s)))}));
	}

	StmtPtr Parser::parse_stmt_continue()
	{
		auto kw = consume();
		ir::StmtContinue s{};
		s.span = kw.span;
		return std::make_unique<Stmt>(
			Stmt(StmtNormal{std::make_unique<ir::Stmt>(ir::Stmt(std::move(s)))}));
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

	StmtPtr Parser::parse_stmt_macro_error()
	{
		auto kw = consume();

		if (cur().is(lex::TokenKind::newline) || cur().is(lex::TokenKind::eof))
		{
			add_error(cur().span, "expected error message expression after 'error'");
			return nullptr;
		}

		auto msg = parse_expr();

		std::optional<std::string> primary_tokens{};
		if (accept(lex::TokenKind::comma))
		{
			if (!cur().is(lex::TokenKind::identifier))
			{
				add_error(cur().span, "expected tokens binding name after ','");
				recover_to_newline();
				return nullptr;
			}
			auto t = consume();
			primary_tokens = std::string(t.lexeme);

			while (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected token after error header");
				advance();
			}
		}

		skip_newlines();

		std::vector<DiagItem> items{};

		auto parse_item = [this](fe::DiagItemKind kind) -> std::optional<fe::DiagItem> {
			auto item_kw = consume();

			if (cur().is(lex::TokenKind::newline) || cur().is(lex::TokenKind::eof))
			{
				add_error(cur().span, "expected message expression");
				return std::nullopt;
			}

			auto msg_expr = parse_expr();

			std::optional<std::string> tokens_name{};
			if (accept(lex::TokenKind::comma))
			{
				if (!cur().is(lex::TokenKind::identifier))
				{
					add_error(cur().span, "expected tokens binding name after ','");
					recover_to_newline();
					return std::nullopt;
				}
				auto tok = consume();
				tokens_name = std::string(tok.lexeme);

				while (!cur().is(lex::TokenKind::newline) && !cur().is(lex::TokenKind::eof))
				{
					add_error(cur().span, "unexpected token after diagnostic item");
					advance();
				}
			}

			fe::DiagItem item{};
			item.span = merge_spans(item_kw.span, msg_expr.span);
			item.kind = kind;
			item.message = std::move(msg_expr);
			item.tokens_name = std::move(tokens_name);
			return item;
		};

		for (;;)
		{
			skip_newlines();

			if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_error))
				break;

			if (cur().is(lex::TokenKind::eof))
			{
				add_error(cur().span, "unexpected end of file in 'error' block");
				break;
			}

			if (cur().is(lex::TokenKind::kw_note))
			{
				if (auto item = parse_item(fe::DiagItemKind::note))
					items.push_back(std::move(*item));
				recover_to_newline();
				continue;
			}

			if (cur().is(lex::TokenKind::kw_help))
			{
				if (auto item = parse_item(fe::DiagItemKind::help))
					items.push_back(std::move(*item));
				recover_to_newline();
				continue;
			}

			if (cur().is(lex::TokenKind::kw_suggestion))
			{
				if (auto item = parse_item(fe::DiagItemKind::suggestion))
					items.push_back(std::move(*item));
				recover_to_newline();
				continue;
			}

			if (cur().is(lex::TokenKind::kw_ref))
			{
				if (auto item = parse_item(fe::DiagItemKind::reference))
					items.push_back(std::move(*item));
				recover_to_newline();
				continue;
			}

			add_error(cur().span, "expected note/help/suggestion/ref or 'end error'");
			recover_to_newline();
		}

		SourceSpan end_span = kw.span;
		if (cur().is(lex::TokenKind::kw_end) && next().is(lex::TokenKind::kw_error))
		{
			auto end_kw = consume();
			auto which = consume();
			end_span = merge_spans(end_kw.span, which.span);
		}

		StmtMacroError st{};
		st.span = merge_spans(kw.span, end_span);
		st.message = std::move(msg);
		st.primary_tokens = std::move(primary_tokens);
		st.items = std::move(items);

		return std::make_unique<Stmt>(Stmt(std::move(st)));
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
