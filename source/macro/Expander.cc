#include <cstdint>
#include <deque>
#include <limits>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>
#include <yasme/Diagnostics.hh>
#include <yasme/fe/Ast.hh>
#include <yasme/fe/Parser.hh>
#include <yasme/ir/ExprParser.hh>
#include <yasme/macro/Expander.hh>
#include <yasme/macro/Pattern.hh>
#include <yasme/macro/TokenSlice.hh>
#include <yasme/support/SourceManager.hh>

namespace yasme::macro
{
	namespace
	{
		template <class... Ts> struct Overload : Ts...
		{
			using Ts::operator()...;
		};
		template <class... Ts> Overload(Ts...) -> Overload<Ts...>;

		struct MacroEnv;

		struct MacroDef
		{
			fe::StmtMacroDef const* def{};
		};

		struct MacroValueUnknown
		{
		};

		struct MacroValueExpr
		{
			ir::Expr expr{};
		};

		struct MacroValueSymbolicParam
		{
			ir::Expr expr{};
		};

		struct MacroValueTokens
		{
			TokenSlice slice{};
			MacroEnv const* origin_env{};
		};

		struct MacroValueRefSymbol
		{
			std::string name{};
		};

		struct MacroValueRefLocal
		{
			MacroEnv* env{};
			std::string name{};
		};

		using MacroValue = std::variant<MacroValueUnknown,
										MacroValueExpr,
										MacroValueSymbolicParam,
										MacroValueTokens,
										MacroValueRefSymbol,
										MacroValueRefLocal>;

		struct MacroEnv
		{
			std::unordered_map<std::string, MacroValue> values{};
			std::unordered_set<std::string> locals{};
			std::unordered_set<std::string> refs{};
			std::unordered_map<std::string, MacroValue> shadowed_locals{};
		};

		struct CallFrame
		{
			std::string name{};
			SourceSpan span{};
			SourceSpan def_span{};
		};

		[[nodiscard]] std::size_t slice_size(TokenSlice slice) noexcept
		{
			if (!slice.begin || !slice.end)
				return 0;
			return static_cast<std::size_t>(slice.end - slice.begin);
		}

		[[nodiscard]] bool slice_is_single_ident(TokenSlice slice, lex::Token& out) noexcept
		{
			if (slice_size(slice) != 1)
				return false;
			auto tok = slice.begin[0];
			if (!tok.is(lex::TokenKind::identifier))
				return false;
			out = tok;
			return true;
		}

		[[nodiscard]] std::string make_macro_sig_error(std::string_view name,
													   std::size_t expected_min,
													   std::size_t expected_max,
													   std::size_t got)
		{
			if (expected_min == expected_max)
			{
				return "macro '" + std::string(name) + "' expects " + std::to_string(expected_min)
					   + " argument(s) but got " + std::to_string(got);
			}

			return "macro '" + std::string(name) + "' expects between "
				   + std::to_string(expected_min) + " and " + std::to_string(expected_max)
				   + " argument(s) but got " + std::to_string(got);
		}
	} // namespace

	class ExpanderImpl
	{
	public:
		struct TokensSubstKey
		{
			MacroEnv const* env{};
			std::string name{};
		};

		explicit ExpanderImpl(SourceManager& sources, Diagnostics& diag) noexcept
			: m_sources(std::addressof(sources)), m_diag(std::addressof(diag))
		{
		}

		[[nodiscard]] ir::Program expand(fe::Program const& program)
		{
			m_loop_depth = 0;
			m_return_depth = 0;
			m_runtime_cf_depth = 0;

			m_macros.clear();
			m_call_stack.clear();
			m_include_stack.clear();
			m_include_programs.clear();
			m_tokens_subst_stack.clear();
			m_call_loop_base_stack.clear();
			m_generated_token_lists.clear();
			m_generated_token_lexemes.clear();

			for (auto const& st : program.stmts)
				collect_macros(*st);

			ir::Program out{};
			for (auto const& st : program.stmts)
				expand_stmt(*st, nullptr, out.stmts);
			return out;
		}

	private:
		void emit_fe_parse_errors(std::vector<fe::ParseError> const& errs)
		{
			for (auto const& e : errs)
				emit_error(e.span, e.message);
		}

		[[nodiscard]] bool push_include(std::string_view key, SourceSpan where)
		{
			if (key.empty())
				return false;

			if (m_include_stack.size() > 128)
			{
				emit_error(where, "include nesting is too deep");
				return false;
			}

			for (auto const& k : m_include_stack)
			{
				if (k == key)
				{
					emit_error(where, "recursive include of '" + std::string(key) + "'");
					return false;
				}
			}

			m_include_stack.push_back(std::string(key));
			return true;
		}

		void pop_include() noexcept
		{
			if (!m_include_stack.empty())
				m_include_stack.pop_back();
		}

		[[nodiscard]] bool
		push_tokens_subst(MacroEnv const* env, std::string_view name, SourceSpan use_span)
		{
			for (auto const& k : m_tokens_subst_stack)
			{
				if (k.env == env && k.name == name)
				{
					emit_error(use_span,
							   "recursive tokens substitution of '" + std::string(name) + "'");
					return false;
				}
			}
			m_tokens_subst_stack.push_back(TokensSubstKey{env, std::string(name)});
			return true;
		}

		void pop_tokens_subst() noexcept
		{
			if (!m_tokens_subst_stack.empty())
				m_tokens_subst_stack.pop_back();
		}

		struct TokensSubstGuard
		{
			ExpanderImpl* self{};
			MacroEnv const* env{};
			bool active{};

			TokensSubstGuard(ExpanderImpl* s,
							 MacroEnv const* e,
							 std::string_view name,
							 SourceSpan use_span)
				: self(s), env(e), active(s && s->push_tokens_subst(e, name, use_span))
			{
			}

			~TokensSubstGuard()
			{
				if (self && active)
					self->pop_tokens_subst();
			}
		};

		[[nodiscard]] bool aborting_current_macro(MacroEnv const* env) const noexcept
		{
			if (!env)
				return false;

			auto const depth = m_call_stack.size();
			return m_return_depth != 0 && m_return_depth == depth;
		}

		struct RuntimeCfGuard
		{
			ExpanderImpl* self{};
			explicit RuntimeCfGuard(ExpanderImpl* s) : self(s)
			{
				if (self)
					++self->m_runtime_cf_depth;
			}
			~RuntimeCfGuard()
			{
				if (self)
					--self->m_runtime_cf_depth;
			}
		};

		[[nodiscard]] std::size_t current_macro_loop_base(MacroEnv const* env) const noexcept
		{
			if (!env)
				return 0;

			if (m_call_loop_base_stack.empty())
				return 0;

			return m_call_loop_base_stack.back();
		}

		struct LoopDepthGuard
		{
			ExpanderImpl* self{};
			explicit LoopDepthGuard(ExpanderImpl* s) : self(s)
			{
				if (self)
					++self->m_loop_depth;
			}
			~LoopDepthGuard()
			{
				if (self)
					--self->m_loop_depth;
			}
		};

		[[nodiscard]] fe::Program const* load_include_program(fe::StmtInclude const& node)
		{
			if (node.file == FileId{} || node.normalized_key.empty())
				return nullptr;

			auto it = m_include_programs.find(node.normalized_key);
			if (it != m_include_programs.end())
				return it->second.get();

			fe::Parser parser(*m_sources, node.file, {}, {});
			auto res = parser.parse_program();
			if (!res.errors.empty())
				emit_fe_parse_errors(res.errors);

			auto p = std::make_unique<fe::Program>(std::move(res.program));
			auto* ptr = p.get();
			m_include_programs.emplace(node.normalized_key, std::move(p));
			return ptr;
		}

		void collect_macros(fe::Stmt const& st)
		{
			std::visit(Overload{
						   [this](fe::StmtMacroDef const& node) { define_macro(node); },
						   [this](fe::StmtVirtual const& node) {
							   for (auto const& nested : node.body)
								   collect_macros(*nested);
						   },
						   [this](fe::StmtPostpone const& node) {
							   for (auto const& nested : node.body)
								   collect_macros(*nested);
						   },
						   [this](fe::StmtIf const& node) {
							   for (auto const& nested : node.then_body)
								   collect_macros(*nested);
							   for (auto const& nested : node.else_body)
								   collect_macros(*nested);
						   },
						   [this](fe::StmtRepeat const& node) {
							   for (auto const& nested : node.body)
								   collect_macros(*nested);
						   },
						   [this](fe::StmtWhile const& node) {
							   for (auto const& nested : node.body)
								   collect_macros(*nested);
						   },
						   [this](fe::StmtForNumeric const& node) {
							   for (auto const& nested : node.body)
								   collect_macros(*nested);
						   },
						   [this](fe::StmtForChars const& node) {
							   for (auto const& nested : node.body)
								   collect_macros(*nested);
						   },
						   [this](fe::StmtInclude const& node) {
							   if (node.normalized_key.empty() || node.file == FileId{})
								   return;

							   if (!push_include(node.normalized_key, node.span))
								   return;

							   auto const* p = load_include_program(node);
							   if (p != nullptr)
								   for (auto const& st2 : p->stmts)
									   collect_macros(*st2);

							   pop_include();
						   },
						   [](auto const&) {},
					   },
					   st.node);
		}

		bool define_macro(fe::StmtMacroDef const& node)
		{
			auto it = m_macros.find(node.name);
			if (it != m_macros.end())
			{
				emit_error(node.span, "redefinition of macro '" + node.name + "'");
				return false;
			}

			m_macros.emplace(node.name, MacroDef{std::addressof(node)});
			return true;
		}

		void emit_error(SourceSpan span, std::string message)
		{
			Diagnostic d{};
			d.level = DiagnosticLevel::error;
			d.message = std::move(message);
			d.primary = span;
			m_diag->emit(d);
		}

		[[nodiscard]] bool
		push_call(std::string_view name, SourceSpan call_span, SourceSpan def_span)
		{
			for (auto const& frame : m_call_stack)
			{
				if (frame.name != name)
					continue;

				Diagnostic d{};
				d.level = DiagnosticLevel::error;
				d.message = "recursive macro expansion of '" + std::string(name) + "'";
				d.primary = call_span;
				for (auto const& stack_frame : m_call_stack)
				{
					d.labels.push_back(
						DiagnosticLabel{"expanded from macro '" + stack_frame.name + "'",
										stack_frame.span,
										LabelKind::note});
				}
				m_diag->emit(d);
				return false;
			}

			m_call_stack.push_back(CallFrame{std::string(name), call_span, def_span});
			m_call_loop_base_stack.push_back(m_loop_depth);
			return true;
		}

		[[nodiscard]] ir::DiagKind to_ir_diag_kind(fe::DiagItemKind k) const noexcept
		{
			switch (k)
			{
				case fe::DiagItemKind::note:
					return ir::DiagKind::note;
				case fe::DiagItemKind::help:
					return ir::DiagKind::help;
				case fe::DiagItemKind::suggestion:
					return ir::DiagKind::suggestion;
				case fe::DiagItemKind::reference:
					return ir::DiagKind::reference;
			}
			return ir::DiagKind::note;
		}

		[[nodiscard]] ir::Expr make_string_expr(SourceSpan sp, std::string text) const
		{
			return ir::Expr(sp, ir::ExprStr{std::move(text)});
		}

		void append_call_stack_context(ir::StmtError& e) const
		{
			for (auto const& frame : m_call_stack)
			{
				if (frame.span.id == 0)
					continue;

				ir::DiagItem it{};
				it.span = frame.span;
				it.kind = ir::DiagKind::reference;
				it.label_span = frame.span;
				it.message =
					make_string_expr(frame.span, "expanded from macro '" + frame.name + "'");
				e.items.push_back(std::move(it));
			}
		}

		void pop_call() noexcept
		{
			if (!m_call_loop_base_stack.empty())
				m_call_loop_base_stack.pop_back();
			if (!m_call_stack.empty())
				m_call_stack.pop_back();
		}

		[[nodiscard]] ir::Expr clone_expr(ir::Expr const& expr) const
		{
			if (auto const* id = std::get_if<ir::ExprIdent>(&expr.node))
				return ir::Expr(expr.span, ir::ExprIdent{id->name});
			if (auto const* val = std::get_if<ir::ExprInt>(&expr.node))
				return ir::Expr(expr.span, ir::ExprInt{val->value});
			if (auto const* val = std::get_if<ir::ExprStr>(&expr.node))
				return ir::Expr(expr.span, ir::ExprStr{val->value});
			if (auto const* val = std::get_if<ir::ExprBuiltin>(&expr.node))
				return ir::Expr(expr.span, ir::ExprBuiltin{val->kind});
			if (auto const* val = std::get_if<ir::ExprUnary>(&expr.node))
			{
				ir::ExprUnary u{};
				u.op = val->op;
				if (val->rhs)
					u.rhs = std::make_unique<ir::Expr>(clone_expr(*val->rhs));
				return ir::Expr(expr.span, std::move(u));
			}
			if (auto const* val = std::get_if<ir::ExprBinary>(&expr.node))
			{
				ir::ExprBinary b{};
				b.op = val->op;
				if (val->lhs)
					b.lhs = std::make_unique<ir::Expr>(clone_expr(*val->lhs));
				if (val->rhs)
					b.rhs = std::make_unique<ir::Expr>(clone_expr(*val->rhs));
				return ir::Expr(expr.span, std::move(b));
			}

			return ir::Expr{};
		}

		[[nodiscard]] MacroValue const* lookup_local_value(MacroEnv const& env,
														   std::string const& name) const noexcept
		{
			if (auto it = env.shadowed_locals.find(name); it != env.shadowed_locals.end())
				return std::addressof(it->second);

			if (auto it = env.values.find(name); it != env.values.end())
				return std::addressof(it->second);

			return nullptr;
		}

		[[nodiscard]] MacroValue* lookup_local_value(MacroEnv& env,
													 std::string const& name) noexcept
		{
			if (auto it = env.shadowed_locals.find(name); it != env.shadowed_locals.end())
				return std::addressof(it->second);

			if (auto it = env.values.find(name); it != env.values.end())
				return std::addressof(it->second);

			return nullptr;
		}

		[[nodiscard]] ir::Expr expand_ref_local_expr(SourceSpan use_span,
													 MacroValueRefLocal const& ref)
		{
			if (!ref.env)
			{
				emit_error(use_span, "internal error: null macro local ref");
				return ir::Expr{};
			}

			auto const* slot = lookup_local_value(*ref.env, ref.name);
			if (!slot)
			{
				emit_error(use_span, "macro local '" + ref.name + "' used before assignment");
				return ir::Expr{};
			}

			if (auto const* val = std::get_if<MacroValueExpr>(slot))
				return clone_expr(val->expr);

			if (std::holds_alternative<MacroValueTokens>(*slot))
			{
				emit_error(use_span,
						   "tokens parameter '" + ref.name + "' cannot be used as an expression");
				return ir::Expr{};
			}

			emit_error(use_span, "macro local '" + ref.name + "' used before assignment");
			return ir::Expr{};
		}

		[[nodiscard]] bool
		try_assign_ref_local(MacroEnv& cur_env, std::string const& ref_param_name, ir::Expr&& rhs)
		{
			if (!cur_env.refs.contains(ref_param_name))
				return false;

			auto it_ref = cur_env.values.find(ref_param_name);
			if (it_ref == cur_env.values.end())
				return false;

			auto const* local_ref = std::get_if<MacroValueRefLocal>(&it_ref->second);
			if (!local_ref || !local_ref->env)
				return false;

			auto* slot = lookup_local_value(*local_ref->env, local_ref->name);
			if (!slot)
				return false;
			*slot = MacroValueExpr{std::move(rhs)};
			return true;
		}

		[[nodiscard]] ir::Expr expand_expr(ir::Expr const& expr, MacroEnv const* env)
		{
			if (!env)
				return clone_expr(expr);

			if (auto const* id = std::get_if<ir::ExprIdent>(&expr.node))
			{
				auto it = env->values.find(id->name);
				if (it == env->values.end())
					return clone_expr(expr);

				if (auto const* val = std::get_if<MacroValueExpr>(&it->second))
					return clone_expr(val->expr);
				if (std::holds_alternative<MacroValueSymbolicParam>(it->second))
					return clone_expr(expr);
				if (auto const* val = std::get_if<MacroValueRefSymbol>(&it->second))
					return ir::Expr(expr.span, ir::ExprIdent{val->name});
				if (auto const* val = std::get_if<MacroValueRefLocal>(&it->second))
					return expand_ref_local_expr(expr.span, *val);

				if (auto const* tokens = std::get_if<MacroValueTokens>(&it->second))
				{
					if (slice_size(tokens->slice) == 0)
					{
						emit_error(expr.span,
								   "tokens binding '" + id->name
									   + "' is empty and cannot be used as an expression");
						return clone_expr(expr);
					}

					TokensSubstGuard guard(this, env, id->name, expr.span);
					if (!guard.active)
						return clone_expr(expr);

					auto parsed = parse_expr(tokens->slice);
					auto const* scope = tokens->origin_env ? tokens->origin_env : env;
					return expand_expr(parsed, scope);
				}

				emit_error(expr.span, "macro local '" + id->name + "' used before assignment");
				return clone_expr(expr);
			}

			if (auto const* u = std::get_if<ir::ExprUnary>(&expr.node))
			{
				ir::ExprUnary out{};
				out.op = u->op;
				if (u->rhs)
					out.rhs = std::make_unique<ir::Expr>(expand_expr(*u->rhs, env));
				return ir::Expr(expr.span, std::move(out));
			}

			if (auto const* b = std::get_if<ir::ExprBinary>(&expr.node))
			{
				ir::ExprBinary out{};
				out.op = b->op;
				if (b->lhs)
					out.lhs = std::make_unique<ir::Expr>(expand_expr(*b->lhs, env));
				if (b->rhs)
					out.rhs = std::make_unique<ir::Expr>(expand_expr(*b->rhs, env));
				return ir::Expr(expr.span, std::move(out));
			}

			return clone_expr(expr);
		}

		[[nodiscard]] ir::Expr parse_expr(TokenSlice slice)
		{
			auto file = slice.span.id;
			return ir::parse_expr_from_tokens(*m_sources, file, slice, *m_diag);
		}

		[[nodiscard]] ir::Expr resolve_expr_arg(TokenSlice slice, MacroEnv const* outer)
		{
			if (outer)
			{
				lex::Token tok{};
				if (slice_is_single_ident(slice, tok))
				{
					auto it = outer->values.find(std::string(tok.lexeme));
					if (it != outer->values.end())
					{
						if (auto const* tokens = std::get_if<MacroValueTokens>(&it->second))
						{
							auto expr = parse_expr(tokens->slice);
							auto const* scope = tokens->origin_env ? tokens->origin_env : outer;
							return expand_expr(expr, scope);
						}
					}
				}
			}

			auto expr = parse_expr(slice);
			return expand_expr(expr, outer);
		}

		[[nodiscard]] MacroValueTokens resolve_tokens_arg(TokenSlice slice, MacroEnv const* outer)
		{
			if (!outer)
				return MacroValueTokens{slice, nullptr};

			auto count = slice_size(slice);
			if (count == 0)
				return MacroValueTokens{slice, outer};

			auto const& first = slice.begin[0];
			if (first.is(lex::TokenKind::identifier))
			{
				auto it = outer->values.find(std::string(first.lexeme));
				if (it != outer->values.end())
				{
					if (auto const* tokens = std::get_if<MacroValueTokens>(&it->second))
					{
						if (count != 1)
						{
							auto sp = (count > 1) ? slice.begin[1].span : slice.span;
							emit_error(sp,
									   "tokens argument '" + std::string(first.lexeme)
										   + "' must be passed by itself");
						}
						return *tokens;
					}
				}
			}

			return MacroValueTokens{slice, outer};
		}

		[[nodiscard]] std::string_view store_generated_lexeme(std::string text)
		{
			m_generated_token_lexemes.push_back(std::move(text));
			return m_generated_token_lexemes.back();
		}

		[[nodiscard]] lex::Token make_token(lex::TokenKind kind, SourceSpan span) noexcept
		{
			lex::Token tok{};
			tok.kind = kind;
			tok.span = span;
			return tok;
		}

		[[nodiscard]] lex::Token make_ident_token(std::string_view name, SourceSpan span)
		{
			lex::Token tok{};
			tok.kind = lex::TokenKind::identifier;
			tok.span = span;
			tok.lexeme = store_generated_lexeme(std::string(name));
			return tok;
		}

		[[nodiscard]] lex::Token make_uint_token(std::uint64_t value, SourceSpan span)
		{
			lex::Token tok{};
			tok.kind = lex::TokenKind::integer;
			tok.span = span;
			tok.lexeme = store_generated_lexeme(std::to_string(value));
			tok.integer.value = value;
			tok.integer.base = lex::NumberBase::decimal;
			tok.integer.overflow = false;
			return tok;
		}

		[[nodiscard]] lex::Token make_int_token(std::int64_t value, SourceSpan span)
		{
			return make_uint_token(static_cast<std::uint64_t>(value), span);
		}

		[[nodiscard]] lex::Token make_str_token(std::string_view value, SourceSpan span)
		{
			lex::Token tok{};
			tok.kind = lex::TokenKind::string;
			tok.span = span;
			tok.lexeme = store_generated_lexeme(std::string(value));
			return tok;
		}

		[[nodiscard]] lex::Token make_unary_token(ir::UnaryOp op, SourceSpan span)
		{
			switch (op)
			{
				case ir::UnaryOp::plus:
					return make_token(lex::TokenKind::plus, span);
				case ir::UnaryOp::minus:
					return make_token(lex::TokenKind::minus, span);
				case ir::UnaryOp::bit_not:
					return make_token(lex::TokenKind::tilde, span);
				case ir::UnaryOp::log_not:
					return make_token(lex::TokenKind::bang, span);
				case ir::UnaryOp::at:
					return make_token(lex::TokenKind::at, span);
			}
			return make_token(lex::TokenKind::invalid, span);
		}

		[[nodiscard]] lex::Token make_binary_token(ir::BinaryOp op, SourceSpan span)
		{
			switch (op)
			{
				case ir::BinaryOp::add:
					return make_token(lex::TokenKind::plus, span);
				case ir::BinaryOp::sub:
					return make_token(lex::TokenKind::minus, span);
				case ir::BinaryOp::mul:
					return make_token(lex::TokenKind::star, span);
				case ir::BinaryOp::div:
					return make_token(lex::TokenKind::slash, span);
				case ir::BinaryOp::mod:
					return make_token(lex::TokenKind::percent, span);
				case ir::BinaryOp::shl:
					return make_token(lex::TokenKind::shl, span);
				case ir::BinaryOp::shr:
					return make_token(lex::TokenKind::shr, span);
				case ir::BinaryOp::bit_and:
					return make_token(lex::TokenKind::amp, span);
				case ir::BinaryOp::bit_or:
					return make_token(lex::TokenKind::pipe, span);
				case ir::BinaryOp::bit_xor:
					return make_token(lex::TokenKind::caret, span);
				case ir::BinaryOp::log_and:
					return make_token(lex::TokenKind::andand, span);
				case ir::BinaryOp::log_or:
					return make_token(lex::TokenKind::oror, span);
				case ir::BinaryOp::eq:
					return make_token(lex::TokenKind::eqeq, span);
				case ir::BinaryOp::ne:
					return make_token(lex::TokenKind::ne, span);
				case ir::BinaryOp::lt:
					return make_token(lex::TokenKind::lt, span);
				case ir::BinaryOp::le:
					return make_token(lex::TokenKind::le, span);
				case ir::BinaryOp::gt:
					return make_token(lex::TokenKind::gt, span);
				case ir::BinaryOp::ge:
					return make_token(lex::TokenKind::ge, span);
				case ir::BinaryOp::concat:
					return make_token(lex::TokenKind::hash, span);
			}
			return make_token(lex::TokenKind::invalid, span);
		}

		void append_expr_tokens(std::vector<lex::Token>& out, ir::Expr const& expr)
		{
			std::visit(Overload{
						   [&](ir::ExprIdent const& id) {
							   out.push_back(make_ident_token(id.name, expr.span));
						   },
						   [&](ir::ExprInt const& i) {
							   if (i.value < 0)
							   {
								   auto abs = (i.value == std::numeric_limits<std::int64_t>::min())
												  ? static_cast<std::uint64_t>(
														std::numeric_limits<std::int64_t>::max())
														+ 1
												  : static_cast<std::uint64_t>(-i.value);

								   out.push_back(make_token(lex::TokenKind::lparen, expr.span));
								   out.push_back(make_token(lex::TokenKind::minus, expr.span));
								   out.push_back(make_uint_token(abs, expr.span));
								   out.push_back(make_token(lex::TokenKind::rparen, expr.span));
							   }
							   else
							   {
								   out.push_back(make_int_token(i.value, expr.span));
							   }
						   },
						   [&](ir::ExprStr const& s) {
							   out.push_back(make_str_token(s.value, expr.span));
						   },
						   [&](ir::ExprBuiltin const& b) {
							   out.push_back(make_token(lex::TokenKind::lparen, expr.span));
							   switch (b.kind)
							   {
								   case ir::BuiltinKind::dollar_address:
									   out.push_back(make_token(lex::TokenKind::dollar, expr.span));
									   break;
								   case ir::BuiltinKind::stream_offset:
									   out.push_back(make_token(lex::TokenKind::at, expr.span));
									   out.push_back(make_token(lex::TokenKind::dollar, expr.span));
									   break;
							   }
							   out.push_back(make_token(lex::TokenKind::rparen, expr.span));
						   },
						   [&](ir::ExprUnary const& u) {
							   out.push_back(make_token(lex::TokenKind::lparen, expr.span));
							   out.push_back(make_unary_token(u.op, expr.span));
							   if (u.rhs)
								   append_expr_tokens(out, *u.rhs);
							   out.push_back(make_token(lex::TokenKind::rparen, expr.span));
						   },
						   [&](ir::ExprBinary const& b) {
							   out.push_back(make_token(lex::TokenKind::lparen, expr.span));
							   if (b.lhs)
								   append_expr_tokens(out, *b.lhs);
							   out.push_back(make_binary_token(b.op, expr.span));
							   if (b.rhs)
								   append_expr_tokens(out, *b.rhs);
							   out.push_back(make_token(lex::TokenKind::rparen, expr.span));
						   },
					   },
					   expr.node);
		}

		[[nodiscard]] TokenSlice store_generated_tokens(std::vector<lex::Token>&& tokens)
		{
			m_generated_token_lists.push_back(std::move(tokens));
			auto const& stored = m_generated_token_lists.back();
			return make_token_slice(stored.data(), stored.data() + stored.size());
		}

		[[nodiscard]] bool append_eval_splice_tokens(std::vector<lex::Token>& out,
													 std::string const& name,
													 SourceSpan span,
													 MacroEnv const& env)
		{
			auto it = env.values.find(name);
			if (it == env.values.end())
			{
				emit_error(span, "unknown binding '" + name + "' in eval pattern");
				return false;
			}

			auto const append_tokens = [&](TokenSlice slice) {
				if (slice.begin && slice.end)
					for (auto it_tok = slice.begin; it_tok != slice.end; ++it_tok)
						out.push_back(*it_tok);
			};

			if (auto const* tokens = std::get_if<MacroValueTokens>(&it->second))
			{
				append_tokens(tokens->slice);
				return true;
			}

			if (auto const* expr = std::get_if<MacroValueExpr>(&it->second))
			{
				append_expr_tokens(out, expr->expr);
				return true;
			}

			if (auto const* expr = std::get_if<MacroValueSymbolicParam>(&it->second))
			{
				append_expr_tokens(out, expr->expr);
				return true;
			}

			if (auto const* ref = std::get_if<MacroValueRefSymbol>(&it->second))
			{
				out.push_back(make_ident_token(ref->name, span));
				return true;
			}

			if (auto const* ref = std::get_if<MacroValueRefLocal>(&it->second))
			{
				auto expr2 = expand_ref_local_expr(span, *ref);
				append_expr_tokens(out, expr2);
				return true;
			}

			emit_error(span, "macro local '" + name + "' used before assignment");
			return false;
		}

		[[nodiscard]] std::optional<TokenSlice> format_eval_pattern(TokenSlice pattern,
																	MacroEnv const& env)
		{
			if (!pattern.begin || !pattern.end)
				return std::nullopt;

			auto* begin = pattern.begin;
			auto* end = pattern.end;
			auto count = static_cast<std::size_t>(end - begin);

			std::vector<lex::Token> out{};
			out.reserve(count);

			for (std::size_t i = 0; i < count; ++i)
			{
				auto const& tok = begin[i];

				if (tok.kind == lex::TokenKind::lbrace)
				{
					if (i + 2 >= count)
					{
						emit_error(tok.span, "expected '{name}' in eval pattern");
						return std::nullopt;
					}

					auto const& name_tok = begin[i + 1];
					if (name_tok.kind != lex::TokenKind::identifier)
					{
						emit_error(name_tok.span, "expected identifier after '{' in eval pattern");
						return std::nullopt;
					}

					if (begin[i + 2].kind != lex::TokenKind::rbrace)
					{
						emit_error(name_tok.span, "expected '}' after eval pattern binding");
						return std::nullopt;
					}

					auto bind_name = std::string(name_tok.lexeme);
					if (!append_eval_splice_tokens(out, bind_name, name_tok.span, env))
						return std::nullopt;

					i += 2;
					continue;
				}

				if (tok.kind == lex::TokenKind::rbrace)
				{
					emit_error(tok.span, "unexpected '}' in eval pattern");
					return std::nullopt;
				}

				if (tok.kind == lex::TokenKind::ellipsis
					|| (tok.kind == lex::TokenKind::identifier && tok.lexeme == "_"))
				{
					emit_error(tok.span, "wildcard/ellipsis not allowed in eval pattern");
					return std::nullopt;
				}

				out.push_back(tok);
			}

			return store_generated_tokens(std::move(out));
		}

		[[nodiscard]] std::vector<TokenSlice> split_call_args(TokenSlice raw,
															  std::size_t split_commas)
		{
			std::vector<TokenSlice> out{};
			if (!raw.begin || !raw.end)
				return out;

			auto count = slice_size(raw);
			if (count == 0)
				return out;

			auto* begin = raw.begin;
			std::size_t arg_start = 0;
			std::size_t splits_left = split_commas;

			int paren = 0;
			int bracket = 0;
			int brace = 0;

			for (std::size_t i = 0; i < count; ++i)
			{
				auto const& tok = begin[i];

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

				if (!tok.is(lex::TokenKind::comma))
					continue;
				if (paren != 0 || bracket != 0 || brace != 0)
					continue;
				if (splits_left == 0)
					continue;

				if (arg_start == i)
					emit_error(tok.span, "empty macro argument");
				else
					out.push_back(make_token_slice(begin + arg_start, begin + i));

				arg_start = i + 1;
				--splits_left;
			}

			if (arg_start < count)
				out.push_back(make_token_slice(begin + arg_start, begin + count));

			return out;
		}

		[[nodiscard]] bool bind_params(fe::StmtMacroCall const& call,
									   fe::StmtMacroDef const& def,
									   MacroEnv* outer,
									   MacroEnv& env)
		{
			std::size_t param_count = def.params.size();
			bool has_tokens =
				param_count > 0 && def.params.back().kind == fe::MacroParamKind::tokens;

			std::size_t min_args = param_count;
			if (has_tokens)
				min_args = (param_count == 1) ? 0 : (param_count - 1);

			std::size_t max_args = param_count;

			auto split_commas = has_tokens ? (param_count > 0 ? (param_count - 1) : 0)
										   : std::numeric_limits<std::size_t>::max();

			auto call_args = split_call_args(call.raw_args, split_commas);

			if (call_args.size() < min_args || call_args.size() > max_args)
			{
				emit_error(call.span,
						   make_macro_sig_error(def.name, min_args, max_args, call_args.size()));
				return false;
			}

			for (std::size_t i = 0; i < param_count; ++i)
			{
				auto const& param = def.params[i];
				auto arg_index = i;

				if (param.kind == fe::MacroParamKind::tokens)
				{
					MacroValueTokens tokens_val{};
					if (arg_index < call_args.size())
						tokens_val = resolve_tokens_arg(call_args[arg_index], outer);
					else
						tokens_val = MacroValueTokens{{}, outer};

					env.values[param.name] = tokens_val;
					continue;
				}

				if (arg_index >= call_args.size())
				{
					emit_error(param.span, "missing argument for parameter '" + param.name + "'");
					return false;
				}

				auto const slice = call_args[arg_index];

				if (param.kind == fe::MacroParamKind::ref)
				{
					lex::Token tok{};
					if (!slice_is_single_ident(slice, tok))
					{
						emit_error(slice.span,
								   "ref parameter '" + param.name
									   + "' requires a single identifier argument");
						return false;
					}

					std::string target = std::string(tok.lexeme);
					if (outer)
					{
						auto it = outer->values.find(target);
						if (it != outer->values.end())
						{
							if (auto const* ref = std::get_if<MacroValueRefSymbol>(&it->second))
								target = ref->name;
							else if (auto const* ref = std::get_if<MacroValueRefLocal>(&it->second))
							{
								env.values[param.name] = MacroValueRefLocal{ref->env, ref->name};
								env.refs.insert(param.name);
								continue;
							}
							else if (outer->locals.contains(target))
							{
								env.values[param.name] = MacroValueRefLocal{outer, target};
								env.refs.insert(param.name);
								continue;
							}
							else
							{
								emit_error(slice.span,
										   "ref argument '" + target
											   + "' does not name a ref binding");
							}
						}
					}

					env.values[param.name] = MacroValueRefSymbol{target};
					env.refs.insert(param.name);
					continue;
				}

				auto expr = resolve_expr_arg(slice, outer);
				if (param.kind == fe::MacroParamKind::name && !ir::is_name_expr(expr))
				{
					emit_error(slice.span,
							   "parameter '" + param.name
								   + "' requires a name expression argument");
				}

				if (param.kind == fe::MacroParamKind::name)
					env.values[param.name] = MacroValueSymbolicParam{std::move(expr)};
				else
					env.values[param.name] = MacroValueExpr{std::move(expr)};
			}

			return true;
		}

		void expand_stmt(fe::Stmt const& st, MacroEnv* env, std::vector<ir::StmtPtr>& out)
		{
			if (aborting_current_macro(env))
				return;

			std::visit(
				Overload{
					[this, env](fe::StmtMacroDef const& node) {
						if (env)
							define_macro(node);
					},
					[this, env, &out](fe::StmtInclude const& node) {
						if (node.normalized_key.empty() || node.file == FileId{})
							return;

						if (!push_include(node.normalized_key, node.span))
							return;

						auto const* p = load_include_program(node);
						if (p != nullptr)
							for (auto const& st2 : p->stmts)
							{
								expand_stmt(*st2, env, out);
								if (aborting_current_macro(env))
									break;
							}

						pop_include();
					},
					[this, env, &out](fe::StmtMacroCall const& node) {
						auto scope = expand_macro_call(node, env);
						if (scope)
							out.push_back(std::move(scope));
					},
					[this, env, &out](fe::StmtMacroError const& node) {
						if (!env)
						{
							emit_error(node.span, "'error' is only valid inside macro bodies");
							return;
						}

						ir::StmtError e{};
						e.span = node.span;
						e.message = expand_expr(node.message, env);

						auto add_note = [&](SourceSpan sp, std::string text) {
							ir::DiagItem it{};
							it.span = sp;
							it.kind = ir::DiagKind::note;
							it.message = make_string_expr(sp, std::move(text));
							e.items.push_back(std::move(it));
						};

						SourceSpan primary = node.span;
						if (node.primary_tokens)
							if (auto sp = try_tokens_span(*node.primary_tokens, *env))
								primary = *sp;
							else
								add_note(node.span,
										 "unknown tokens binding '" + *node.primary_tokens
											 + "' for error span");
						else if (!m_call_stack.empty() && m_call_stack.back().span.id != 0)
							primary = m_call_stack.back().span;
						e.primary = primary;

						for (auto const& item : node.items)
						{
							ir::DiagItem it{};
							it.span = item.span;
							it.kind = to_ir_diag_kind(item.kind);
							it.message = expand_expr(item.message, env);

							if (item.tokens_name)
							{
								if (auto sp = try_tokens_span(*item.tokens_name, *env))
								{
									it.label_span = *sp;
								}
								else
								{
									add_note(item.span,
											 "unknown tokens binding '" + *item.tokens_name
												 + "' for diagnostic item");
								}
							}

							e.items.push_back(std::move(it));
						}

						append_call_stack_context(e);

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(e))));
					},
					[this, env](fe::StmtLocal const& node) {
						if (!env)
						{
							emit_error(node.span, "'local' is only valid inside macro bodies");
							return;
						}

						for (auto const& name : node.names)
						{
							if (env->values.contains(name))
							{
								emit_error(node.span,
										   "local name '" + name
											   + "' conflicts with an existing binding");
								continue;
							}
							env->locals.insert(name);
							env->values.emplace(name, MacroValueUnknown{});
						}
					},
					[this, env, &out](fe::StmtEval const& node) {
						if (!env)
						{
							emit_error(node.span, "'eval' is only valid inside macro bodies");
							return;
						}

						TokenSlice tokens_slice{};
						MacroEnv const* origin_env = env;

						if (node.input_kind == fe::StmtEval::InputKind::tokens_name)
						{
							auto it = env->values.find(node.tokens_name);
							if (it == env->values.end())
							{
								emit_error(node.span,
										   "unknown tokens parameter '" + node.tokens_name + "'");
								return;
							}

							auto const* tokens = std::get_if<MacroValueTokens>(&it->second);
							if (!tokens)
							{
								emit_error(node.span,
										   "'" + node.tokens_name + "' is not a tokens parameter");
								return;
							}

							tokens_slice = tokens->slice;
							origin_env = tokens->origin_env ? tokens->origin_env : env;
						}
						else
						{
							auto formatted = format_eval_pattern(node.pattern, *env);
							if (!formatted)
								return;

							tokens_slice = *formatted;
						}

						if (node.output_kind == fe::StmtEval::OutputKind::tokens)
						{
							if (env->refs.contains(node.out_name))
							{
								emit_error(node.span,
										   "cannot assign tokens to ref parameter '" + node.out_name
											   + "'");
								return;
							}

							MacroValueTokens tokens_val{tokens_slice, origin_env};
							if (env->locals.contains(node.out_name))
								if (auto* slot = lookup_local_value(*env, node.out_name))
									*slot = tokens_val;
								else
									env->values[node.out_name] = tokens_val;
							else
								env->values[node.out_name] = tokens_val;
							return;
						}

						auto expr = parse_expr(tokens_slice);
						expr = expand_expr(expr, origin_env);

						if (env->locals.contains(node.out_name))
						{
							if (auto* slot = lookup_local_value(*env, node.out_name))
								*slot = MacroValueExpr{std::move(expr)};
							else
								env->values[node.out_name] = MacroValueExpr{std::move(expr)};
							return;
						}

						if (try_assign_ref_local(*env, node.out_name, clone_expr(expr)))
							return;

						std::string out_name = node.out_name;
						if (env->refs.contains(out_name))
							if (auto it_ref = env->values.find(out_name);
								it_ref != env->values.end())
								if (auto const* ref =
										std::get_if<MacroValueRefSymbol>(&it_ref->second))
									out_name = ref->name;

						ir::StmtAssign assign{};
						assign.span = node.span;
						assign.name = std::move(out_name);
						assign.rhs = std::move(expr);
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(assign))));
					},
					[this, env, &out](fe::StmtIf const& node) {
						ir::StmtIf s{};
						s.span = node.span;
						s.cond = expand_expr(node.cond, env);
						s.has_else = node.has_else;

						RuntimeCfGuard cf(this);
						for (auto const& st2 : node.then_body)
						{
							expand_stmt(*st2, env, s.then_body);
							if (aborting_current_macro(env))
								break;
						}

						for (auto const& st2 : node.else_body)
						{
							expand_stmt(*st2, env, s.else_body);
							if (aborting_current_macro(env))
								break;
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](fe::StmtRepeat const& node) {
						ir::StmtRepeat s{};
						s.span = node.span;
						s.count = expand_expr(node.count, env);

						RuntimeCfGuard cf(this);
						LoopDepthGuard guard(this);
						for (auto const& st2 : node.body)
						{
							expand_stmt(*st2, env, s.body);
							if (aborting_current_macro(env))
								break;
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](fe::StmtWhile const& node) {
						ir::StmtWhile s{};
						s.span = node.span;
						s.cond = expand_expr(node.cond, env);

						RuntimeCfGuard cf(this);
						LoopDepthGuard guard(this);
						for (auto const& st2 : node.body)
						{
							expand_stmt(*st2, env, s.body);
							if (aborting_current_macro(env))
								break;
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](fe::StmtForNumeric const& node) {
						ir::StmtForNumeric s{};
						s.span = node.span;
						s.var = node.var;
						s.start = expand_expr(node.start, env);
						s.end = expand_expr(node.end, env);
						if (node.step)
							s.step = expand_expr(*node.step, env);

						RuntimeCfGuard cf(this);
						LoopDepthGuard guard(this);
						for (auto const& st2 : node.body)
						{
							expand_stmt(*st2, env, s.body);
							if (aborting_current_macro(env))
								break;
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](fe::StmtForChars const& node) {
						ir::StmtForChars s{};
						s.span = node.span;
						s.var = node.var;
						s.str = expand_expr(node.str, env);

						RuntimeCfGuard cf(this);
						LoopDepthGuard guard(this);
						for (auto const& st2 : node.body)
						{
							expand_stmt(*st2, env, s.body);
							if (aborting_current_macro(env))
								break;
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](fe::StmtMatch const& node) {
						if (!env)
						{
							emit_error(node.span, "'match' is only valid inside macro bodies");
							return;
						}

						auto it = env->values.find(node.tokens_name);
						if (it == env->values.end())
						{
							emit_error(node.span,
									   "unknown tokens parameter '" + node.tokens_name + "'");
							return;
						}

						auto const* tokens = std::get_if<MacroValueTokens>(&it->second);
						if (!tokens)
						{
							emit_error(node.span,
									   "'" + node.tokens_name + "' is not a tokens parameter");
							return;
						}

						auto parsed = parse_pattern(node.pattern);
						for (auto const& err : parsed.errors)
							emit_error(err.span, err.message);
						if (!parsed.ok())
							return;

						MatchResult result{};
						if (match_pattern(parsed.pattern, tokens->slice, result))
						{
							enum class RestoreKind
							{
								restore_value,
								erase_value,
								restore_shadowed_local,
							};

							struct Restore
							{
								std::string name{};
								RestoreKind kind{RestoreKind::erase_value};
								MacroValue value{};
							};

							std::vector<Restore> restores{};
							restores.reserve(result.bindings.size());
							for (auto const& [name, slice] : result.bindings)
							{
								auto it_value = env->values.find(name);
								Restore r{};
								r.name = name;
								auto const is_local = env->locals.contains(name);
								auto const is_unshadowed_local =
									is_local && !env->shadowed_locals.contains(name);

								if (is_unshadowed_local)
								{
									if (it_value != env->values.end())
									{
										env->shadowed_locals.emplace(name,
																	 std::move(it_value->second));
										env->values.erase(it_value);
									}
									else
									{
										env->shadowed_locals.emplace(name, MacroValueUnknown{});
									}

									r.kind = RestoreKind::restore_shadowed_local;
								}
								else if (it_value != env->values.end())
								{
									r.kind = RestoreKind::restore_value;
									r.value = std::move(it_value->second);
								}
								else
								{
									r.kind = RestoreKind::erase_value;
								}

								restores.push_back(std::move(r));
								env->values[name] = MacroValueTokens{slice, tokens->origin_env};
							}

							for (auto const& st2 : node.on_match)
							{
								expand_stmt(*st2, env, out);
								if (aborting_current_macro(env))
									break;
							}

							for (auto it_restore = restores.rbegin(); it_restore != restores.rend();
								 ++it_restore)
							{
								switch (it_restore->kind)
								{
									case RestoreKind::restore_value:
										env->values[it_restore->name] =
											std::move(it_restore->value);
										break;
									case RestoreKind::erase_value:
										env->values.erase(it_restore->name);
										break;
									case RestoreKind::restore_shadowed_local: {
										env->values.erase(it_restore->name);
										if (auto it_local =
												env->shadowed_locals.find(it_restore->name);
											it_local != env->shadowed_locals.end())
										{
											env->values[it_restore->name] =
												std::move(it_local->second);
											env->shadowed_locals.erase(it_local);
										}
										else
										{
											env->values[it_restore->name] = MacroValueUnknown{};
										}
									}
									break;
								}
							}
						}
						else if (node.has_else)
						{
							for (auto const& st2 : node.on_else)
							{
								expand_stmt(*st2, env, out);
								if (aborting_current_macro(env))
									break;
							}
						}
					},
					[this, env, &out](fe::StmtVirtual const& node) {
						ir::StmtVirtual virt{};
						virt.span = node.span;
						if (node.name_expr)
							virt.name_expr = expand_expr(*node.name_expr, env);

						for (auto const& st2 : node.body)
						{
							expand_stmt(*st2, env, virt.body);
							if (aborting_current_macro(env))
								break;
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(virt))));
					},
					[this, env, &out](fe::StmtPostpone const& node) {
						ir::StmtPostpone postpone{};
						postpone.span = node.span;
						postpone.mode = node.mode;

						for (auto const& st2 : node.body)
						{
							expand_stmt(*st2, env, postpone.body);
							if (aborting_current_macro(env))
								break;
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(postpone))));
					},
					[this, env, &out](fe::StmtNormal const& node) {
						expand_normal(*node.stmt, env, out);
					},
					[](auto const&) {},
				},
				st.node);
		}

		void expand_normal(ir::Stmt const& st, MacroEnv* env, std::vector<ir::StmtPtr>& out)
		{
			if (aborting_current_macro(env))
				return;

			std::visit(
				Overload{
					[this, env, &out](ir::StmtOrg const& node) {
						ir::StmtOrg s{};
						s.span = node.span;
						s.address = expand_expr(node.address, env);
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[&out](ir::StmtLabel const& node) {
						ir::StmtLabel s{};
						s.span = node.span;
						s.name = node.name;
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtAssign const& node) {
						auto rhs = expand_expr(node.rhs, env);
						if (env && env->locals.contains(node.name))
						{
							if (auto* slot = lookup_local_value(*env, node.name))
								*slot = MacroValueExpr{std::move(rhs)};
							else
								env->values[node.name] = MacroValueExpr{std::move(rhs)};
							return;
						}

						if (env && try_assign_ref_local(*env, node.name, clone_expr(rhs)))
							return;

						std::string name = node.name;
						if (env && env->refs.contains(node.name))
						{
							auto it_ref = env->values.find(node.name);
							if (it_ref != env->values.end())
							{
								if (auto const* ref =
										std::get_if<MacroValueRefSymbol>(&it_ref->second))
									name = ref->name;
								else if (std::holds_alternative<MacroValueRefLocal>(it_ref->second))
								{
									emit_error(node.span,
											   "cannot emit assignment to macro local via ref");
									return;
								}
							}
						}

						ir::StmtAssign s{};
						s.span = node.span;
						s.name = std::move(name);
						s.rhs = std::move(rhs);
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtDefine const& node) {
						ir::StmtDefine s{};
						s.span = node.span;
						s.name = node.name;
						s.value = expand_expr(node.value, env);
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtEmitData const& node) {
						ir::StmtEmitData s{};
						s.span = node.span;
						s.unit = node.unit;
						s.items.reserve(node.items.size());
						for (auto const& item : node.items)
							s.items.push_back(expand_expr(item, env));
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtLoad const& node) {
						ir::StmtLoad s{};
						s.span = node.span;
						s.unit = node.unit;

						std::string name = node.dest;
						if (env && env->refs.contains(node.dest))
						{
							auto it_ref = env->values.find(node.dest);
							if (it_ref != env->values.end())
							{
								if (auto const* ref =
										std::get_if<MacroValueRefSymbol>(&it_ref->second))
								{
									name = ref->name;
								}
								else if (std::holds_alternative<MacroValueRefLocal>(it_ref->second))
								{
									emit_error(node.span,
											   "ref destination refers to a macro local and "
											   "cannot be used as a load destination");
									return;
								}
							}
						}

						s.dest = std::move(name);
						s.stream = expand_expr(node.stream, env);
						s.offset = expand_expr(node.offset, env);
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[&out](ir::StmtEnd const& node) {
						ir::StmtEnd s{};
						s.span = node.span;
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtError const& node) {
						ir::StmtError e{};
						e.span = node.span;
						e.primary = node.primary;
						e.message = expand_expr(node.message, env);
						e.items.reserve(node.items.size());
						for (auto const& item : node.items)
						{
							ir::DiagItem it{};
							it.span = item.span;
							it.kind = item.kind;
							it.label_span = item.label_span;
							it.message = expand_expr(item.message, env);
							e.items.push_back(std::move(it));
						}
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(e))));
					},
					[this, env, &out](ir::StmtIf const& node) {
						ir::StmtIf s{};
						s.span = node.span;
						s.cond = expand_expr(node.cond, env);
						s.has_else = node.has_else;

						RuntimeCfGuard cf(this);

						for (auto const& st2 : node.then_body)
						{
							if (!st2)
								continue;
							expand_normal(*st2, env, s.then_body);
							if (aborting_current_macro(env))
								break;
						}

						for (auto const& st2 : node.else_body)
						{
							if (!st2)
								continue;
							expand_normal(*st2, env, s.else_body);
							if (aborting_current_macro(env))
								break;
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtRepeat const& node) {
						ir::StmtRepeat s{};
						s.span = node.span;
						s.count = expand_expr(node.count, env);

						RuntimeCfGuard cf(this);
						LoopDepthGuard guard(this);
						for (auto const& st2 : node.body)
						{
							if (!st2)
								continue;
							expand_normal(*st2, env, s.body);
							if (aborting_current_macro(env))
								break;
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtWhile const& node) {
						ir::StmtWhile s{};
						s.span = node.span;
						s.cond = expand_expr(node.cond, env);

						RuntimeCfGuard cf(this);
						LoopDepthGuard guard(this);
						for (auto const& st2 : node.body)
						{
							if (!st2)
								continue;
							expand_normal(*st2, env, s.body);
							if (aborting_current_macro(env))
								break;
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtForNumeric const& node) {
						ir::StmtForNumeric s{};
						s.span = node.span;
						s.var = node.var;
						s.start = expand_expr(node.start, env);
						s.end = expand_expr(node.end, env);
						if (node.step)
							s.step = expand_expr(*node.step, env);

						RuntimeCfGuard cf(this);
						LoopDepthGuard guard(this);
						for (auto const& st2 : node.body)
						{
							if (!st2)
								continue;
							expand_normal(*st2, env, s.body);
							if (aborting_current_macro(env))
								break;
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtForChars const& node) {
						ir::StmtForChars s{};
						s.span = node.span;
						s.var = node.var;
						s.str = expand_expr(node.str, env);

						RuntimeCfGuard cf(this);
						LoopDepthGuard guard(this);
						for (auto const& st2 : node.body)
						{
							if (!st2)
								continue;
							expand_normal(*st2, env, s.body);
							if (aborting_current_macro(env))
								break;
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtBreak const& node) {
						if (env)
						{
							auto const base = current_macro_loop_base(env);
							if (m_loop_depth > base)
							{
								ir::StmtBreak s{};
								s.span = node.span;
								out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
								return;
							}

							ir::StmtMacroReturn s{};
							s.span = node.span;
							out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));

							if (m_runtime_cf_depth == 0)
								m_return_depth = m_call_stack.size();
							return;
						}

						if (m_loop_depth != 0)
						{
							ir::StmtBreak s{};
							s.span = node.span;
							out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
							return;
						}

						emit_error(node.span, "'break' outside of a loop");
					},
					[this, env, &out](ir::StmtContinue const& node) {
						if (env)
						{
							auto const base = current_macro_loop_base(env);
							if (m_loop_depth > base)
							{
								ir::StmtContinue s{};
								s.span = node.span;
								out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
								return;
							}

							emit_error(node.span, "'continue' outside of a loop");
							return;
						}

						if (m_loop_depth != 0)
						{
							ir::StmtContinue s{};
							s.span = node.span;
							out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
							return;
						}

						emit_error(node.span, "'continue' outside of a loop");
					},
					[](auto const&) {},
				},
				st.node);
		}

		[[nodiscard]] ir::StmtPtr expand_macro_call(fe::StmtMacroCall const& call, MacroEnv* outer)
		{
			std::vector<ir::StmtPtr> body{};

			auto it = m_macros.find(call.callee);
			if (it == m_macros.end())
			{
				emit_error(call.span, "unknown macro '" + call.callee + "'");
				return nullptr;
			}

			if (!push_call(call.callee, call.span, it->second.def->span))
				return nullptr;

			auto const depth = m_call_stack.size();

			MacroEnv env{};
			if (!bind_params(call, *it->second.def, outer, env))
			{
				pop_call();
				return nullptr;
			}

			for (auto const& param : it->second.def->params)
			{
				if (param.kind != fe::MacroParamKind::name)
					continue;

				auto it_value = env.values.find(param.name);
				if (it_value == env.values.end())
					continue;

				auto const* binding = std::get_if<MacroValueSymbolicParam>(&it_value->second);
				if (!binding)
					continue;

				ir::StmtAssign s{};
				s.span = call.span;
				s.name = param.name;
				s.rhs = clone_expr(binding->expr);
				body.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
			}

			for (auto const& st : it->second.def->body)
			{
				expand_stmt(*st, &env, body);
				if (aborting_current_macro(&env))
					break;
			}

			if (m_return_depth != 0 && m_return_depth == depth)
				m_return_depth = 0;

			pop_call();

			ir::StmtMacroScope scope{};
			scope.span = call.span;
			scope.body = std::move(body);
			return std::make_unique<ir::Stmt>(ir::Stmt(std::move(scope)));
		}

		[[nodiscard]] std::optional<SourceSpan> try_tokens_span(std::string const& name,
																MacroEnv const& env) const
		{
			auto it = env.values.find(name);
			if (it == env.values.end())
				return std::nullopt;

			auto const* tokens = std::get_if<MacroValueTokens>(&it->second);
			if (!tokens)
				return std::nullopt;

			return tokens->slice.span;
		}

	private:
		SourceManager* m_sources{};
		Diagnostics* m_diag{};
		std::unordered_map<std::string, MacroDef> m_macros{};
		std::vector<CallFrame> m_call_stack{};
		std::vector<TokensSubstKey> m_tokens_subst_stack{};
		std::vector<std::string> m_include_stack{};
		std::unordered_map<std::string, std::unique_ptr<fe::Program>> m_include_programs{};
		std::vector<std::size_t> m_call_loop_base_stack{};
		std::size_t m_loop_depth{};
		std::size_t m_return_depth{};
		std::size_t m_runtime_cf_depth{};
		std::vector<std::vector<lex::Token>> m_generated_token_lists{};
		std::deque<std::string> m_generated_token_lexemes{};
	};

	Expander::Expander(SourceManager& sources, Diagnostics& diag) noexcept
		: m_sources(std::addressof(sources)), m_diag(std::addressof(diag))
	{
	}

	ir::Program Expander::expand(fe::Program const& program)
	{
		ExpanderImpl impl(*m_sources, *m_diag);
		return impl.expand(program);
	}

} // namespace yasme::macro
