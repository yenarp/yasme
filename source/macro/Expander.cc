#include <memory>
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

		struct MacroValueTokens
		{
			TokenSlice slice{};
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
										MacroValueTokens,
										MacroValueRefSymbol,
										MacroValueRefLocal>;

		struct MacroEnv
		{
			std::unordered_map<std::string, MacroValue> values{};
			std::unordered_set<std::string> locals{};
			std::unordered_set<std::string> refs{};
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
		explicit ExpanderImpl(SourceManager& sources, Diagnostics& diag) noexcept
			: m_sources(std::addressof(sources)), m_diag(std::addressof(diag))
		{
		}

		[[nodiscard]] ir::Program expand(fe::Program const& program)
		{
			m_macros.clear();
			m_call_stack.clear();
			m_include_stack.clear();
			m_include_programs.clear();

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
			return true;
		}

		void pop_call() noexcept
		{
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

		[[nodiscard]] ir::Expr expand_ref_local_expr(SourceSpan use_span,
													 MacroValueRefLocal const& ref)
		{
			if (!ref.env)
			{
				emit_error(use_span, "internal error: null macro local ref");
				return ir::Expr{};
			}

			auto it = ref.env->values.find(ref.name);
			if (it == ref.env->values.end())
			{
				emit_error(use_span, "macro local '" + ref.name + "' used before assignment");
				return ir::Expr{};
			}

			if (auto const* val = std::get_if<MacroValueExpr>(&it->second))
				return clone_expr(val->expr);

			if (std::holds_alternative<MacroValueTokens>(it->second))
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

			local_ref->env->values[local_ref->name] = MacroValueExpr{std::move(rhs)};
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
				if (auto const* val = std::get_if<MacroValueRefSymbol>(&it->second))
					return ir::Expr(expr.span, ir::ExprIdent{val->name});
				if (auto const* val = std::get_if<MacroValueRefLocal>(&it->second))
					return expand_ref_local_expr(expr.span, *val);
				if (std::holds_alternative<MacroValueTokens>(it->second))
				{
					emit_error(expr.span,
							   "tokens parameter '" + id->name
								   + "' cannot be used as an expression");
					return clone_expr(expr);
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
							return expand_expr(expr, outer);
						}
					}
				}
			}

			auto expr = parse_expr(slice);
			return expand_expr(expr, outer);
		}

		[[nodiscard]] TokenSlice resolve_tokens_arg(TokenSlice slice, MacroEnv const* outer)
		{
			if (!outer)
				return slice;

			lex::Token tok{};
			if (!slice_is_single_ident(slice, tok))
				return slice;

			auto it = outer->values.find(std::string(tok.lexeme));
			if (it == outer->values.end())
				return slice;

			if (auto const* tokens = std::get_if<MacroValueTokens>(&it->second))
				return tokens->slice;

			return slice;
		}

		[[nodiscard]] bool bind_params(fe::StmtMacroCall const& call,
									   fe::StmtMacroDef const& def,
									   MacroEnv* outer,
									   MacroEnv& env)
		{
			std::size_t param_count = def.params.size();
			bool has_tokens =
				param_count > 0 && def.params.back().kind == fe::MacroParamKind::tokens;
			std::size_t min_args = has_tokens ? (param_count - 1) : param_count;
			std::size_t max_args = param_count;

			if (call.args.size() < min_args || call.args.size() > max_args)
			{
				emit_error(call.span,
						   make_macro_sig_error(def.name, min_args, max_args, call.args.size()));
				return false;
			}

			for (std::size_t i = 0; i < param_count; ++i)
			{
				auto const& param = def.params[i];
				auto arg_index = i;

				if (param.kind == fe::MacroParamKind::tokens)
				{
					TokenSlice slice{};
					if (arg_index < call.args.size())
						slice = resolve_tokens_arg(call.args[arg_index], outer);
					env.values[param.name] = MacroValueTokens{slice};
					continue;
				}

				if (arg_index >= call.args.size())
				{
					emit_error(param.span, "missing argument for parameter '" + param.name + "'");
					return false;
				}

				auto const slice = call.args[arg_index];

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

				env.values[param.name] = MacroValueExpr{std::move(expr)};
			}

			return true;
		}

		void expand_stmt(fe::Stmt const& st, MacroEnv* env, std::vector<ir::StmtPtr>& out)
		{
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
								expand_stmt(*st2, env, out);

						pop_include();
					},
					[this, env, &out](fe::StmtMacroCall const& node) {
						auto expanded = expand_macro_call(node, env);
						for (auto& st2 : expanded)
							out.push_back(std::move(st2));
					},
					[this, env](fe::StmtMacroError const& node) {
						if (!env)
						{
							emit_error(node.span, "'error' is only valid inside macro bodies");
							return;
						}

						auto msg_expr = expand_expr(node.message, env);
						auto msg = try_string_value(msg_expr);
						if (!msg)
						{
							emit_error(node.message.span, "macro 'error' message must be a string");
							msg = std::string("macro expansion failed");
						}

						SourceSpan primary = node.span;
						if (node.primary_tokens)
							if (auto sp = try_tokens_span(*node.primary_tokens, *env))
								primary = *sp;
							else
								emit_error(node.span,
										   "unknown tokens binding '" + *node.primary_tokens
											   + "' for error span");
						else if (!m_call_stack.empty() && m_call_stack.back().span.id != 0)
							primary = m_call_stack.back().span;

						Diagnostic d{};
						d.level = DiagnosticLevel::error;
						d.message = *msg;
						d.primary = primary;

						for (auto const& item : node.items)
						{
							auto item_msg_expr = expand_expr(item.message, env);
							auto item_msg = try_string_value(item_msg_expr);
							if (!item_msg)
							{
								emit_error(item.message.span,
										   "diagnostic item message must be a string");
								continue;
							}

							auto add_label_kind = [](fe::DiagItemKind k) -> LabelKind {
								switch (k)
								{
									case fe::DiagItemKind::note:
										return LabelKind::note;
									case fe::DiagItemKind::help:
										return LabelKind::help;
									case fe::DiagItemKind::suggestion:
										return LabelKind::suggestion;
									case fe::DiagItemKind::reference:
										return LabelKind::reference;
								}
								return LabelKind::note;
							};

							auto add_advice_kind = [](fe::DiagItemKind k) -> AdviceKind {
								switch (k)
								{
									case fe::DiagItemKind::note:
										return AdviceKind::note;
									case fe::DiagItemKind::help:
										return AdviceKind::help;
									case fe::DiagItemKind::suggestion:
										return AdviceKind::suggestion;
									case fe::DiagItemKind::reference:
										return AdviceKind::note;
								}
								return AdviceKind::note;
							};

							if (item.tokens_name)
							{
								if (auto sp = try_tokens_span(*item.tokens_name, *env))
								{
									d.labels.push_back(DiagnosticLabel{
										*item_msg,
										*sp,
										add_label_kind(item.kind),
									});
								}
								else
								{
									emit_error(item.span,
											   "unknown tokens binding '" + *item.tokens_name
												   + "' for diagnostic item");
								}
							}
							else
							{
								d.advices.push_back(DiagnosticAdvice{
									add_advice_kind(item.kind),
									*item_msg,
								});
							}
						}

						attach_call_stack_context(d);
						m_diag->emit(d);

						m_abort_depth = m_call_stack.size();
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

						auto expr = parse_expr(tokens->slice);

						if (env->locals.contains(node.out_name))
						{
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

						for (auto const& st2 : node.then_body)
							expand_stmt(*st2, env, s.then_body);

						for (auto const& st2 : node.else_body)
							expand_stmt(*st2, env, s.else_body);

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](fe::StmtRepeat const& node) {
						ir::StmtRepeat s{};
						s.span = node.span;
						s.count = expand_expr(node.count, env);

						for (auto const& st2 : node.body)
							expand_stmt(*st2, env, s.body);

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](fe::StmtWhile const& node) {
						ir::StmtWhile s{};
						s.span = node.span;
						s.cond = expand_expr(node.cond, env);

						for (auto const& st2 : node.body)
							expand_stmt(*st2, env, s.body);

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

						for (auto const& st2 : node.body)
							expand_stmt(*st2, env, s.body);

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](fe::StmtForChars const& node) {
						ir::StmtForChars s{};
						s.span = node.span;
						s.var = node.var;
						s.str = expand_expr(node.str, env);

						for (auto const& st2 : node.body)
							expand_stmt(*st2, env, s.body);

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
							struct Restore
							{
								std::string name{};
								bool had_value{};
								MacroValue value{};
							};

							std::vector<Restore> restores{};
							restores.reserve(result.bindings.size());
							for (auto const& [name, slice] : result.bindings)
							{
								auto it_value = env->values.find(name);
								Restore r{};
								r.name = name;
								r.had_value = (it_value != env->values.end());
								if (r.had_value)
									r.value = std::move(it_value->second);

								restores.push_back(std::move(r));
								env->values[name] = MacroValueTokens{slice};
							}

							for (auto const& st2 : node.on_match)
								expand_stmt(*st2, env, out);

							for (auto it_restore = restores.rbegin(); it_restore != restores.rend();
								 ++it_restore)
							{
								if (it_restore->had_value)
									env->values[it_restore->name] = std::move(it_restore->value);
								else
									env->values.erase(it_restore->name);
							}
						}
						else if (node.has_else)
						{
							for (auto const& st2 : node.on_else)
								expand_stmt(*st2, env, out);
						}
					},
					[this, env, &out](fe::StmtVirtual const& node) {
						ir::StmtVirtual virt{};
						virt.span = node.span;
						if (node.name_expr)
							virt.name_expr = expand_expr(*node.name_expr, env);

						for (auto const& st2 : node.body)
							expand_stmt(*st2, env, virt.body);

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(virt))));
					},
					[this, env, &out](fe::StmtPostpone const& node) {
						ir::StmtPostpone postpone{};
						postpone.span = node.span;
						postpone.mode = node.mode;

						for (auto const& st2 : node.body)
							expand_stmt(*st2, env, postpone.body);

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
					[this, env, &out](ir::StmtIf const& node) {
						ir::StmtIf s{};
						s.span = node.span;
						s.cond = expand_expr(node.cond, env);
						s.has_else = node.has_else;

						for (auto const& st2 : node.then_body)
						{
							if (!st2)
								continue;

							expand_normal(*st2, env, s.then_body);
						}

						for (auto const& st2 : node.else_body)
						{
							if (!st2)
								continue;

							expand_normal(*st2, env, s.else_body);
						}

						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtRepeat const& node) {
						ir::StmtRepeat s{};
						s.span = node.span;
						s.count = expand_expr(node.count, env);
						for (auto const& st2 : node.body)
						{
							if (!st2)
								continue;

							expand_normal(*st2, env, s.body);
						}
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtWhile const& node) {
						ir::StmtWhile s{};
						s.span = node.span;
						s.cond = expand_expr(node.cond, env);
						for (auto const& st2 : node.body)
						{
							if (!st2)
								continue;

							expand_normal(*st2, env, s.body);
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

						for (auto const& st2 : node.body)
						{
							if (!st2)
								continue;

							expand_normal(*st2, env, s.body);
						}
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[this, env, &out](ir::StmtForChars const& node) {
						ir::StmtForChars s{};
						s.span = node.span;
						s.var = node.var;
						s.str = expand_expr(node.str, env);
						for (auto const& st2 : node.body)
						{
							if (!st2)
								continue;

							expand_normal(*st2, env, s.body);
						}
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[&out](ir::StmtBreak const& node) {
						ir::StmtBreak s{};
						s.span = node.span;
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[&out](ir::StmtContinue const& node) {
						ir::StmtContinue s{};
						s.span = node.span;
						out.push_back(std::make_unique<ir::Stmt>(ir::Stmt(std::move(s))));
					},
					[](auto const&) {},
				},
				st.node);
		}

		[[nodiscard]] std::vector<ir::StmtPtr> expand_macro_call(fe::StmtMacroCall const& call,
																 MacroEnv* outer)
		{
			std::vector<ir::StmtPtr> out{};

			auto it = m_macros.find(call.callee);
			if (it == m_macros.end())
			{
				emit_error(call.span, "unknown macro '" + call.callee + "'");
				return out;
			}

			if (!push_call(call.callee, call.span, it->second.def->span))
				return out;

			MacroEnv env{};
			if (!bind_params(call, *it->second.def, outer, env))
			{
				pop_call();
				return out;
			}

			for (auto const& st : it->second.def->body)
			{
				expand_stmt(*st, &env, out);
				if (m_abort_depth == m_call_stack.size())
					break;
			}

			if (m_abort_depth == m_call_stack.size())
				m_abort_depth = 0;

			pop_call();
			return out;
		}

		[[nodiscard]] std::optional<std::string> try_string_value(ir::Expr const& e) const
		{
			if (auto const* s = std::get_if<ir::ExprStr>(&e.node))
				return s->value;

			return std::nullopt;
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

		void attach_call_stack_context(Diagnostic& d) const
		{
			for (auto const& frame : m_call_stack)
			{
				if (frame.span.id != 0)
				{
					d.labels.push_back(DiagnosticLabel{
						"expanded from macro '" + frame.name + "'",
						frame.span,
						LabelKind::reference,
					});
				}
			}

			if (!m_call_stack.empty())
			{
				auto const& top = m_call_stack.back();
				if (top.def_span.id != 0)
				{
					d.labels.push_back(DiagnosticLabel{
						"macro '" + top.name + "' defined here",
						top.def_span,
						LabelKind::note,
					});
				}
			}
		}

	private:
		SourceManager* m_sources{};
		Diagnostics* m_diag{};
		std::unordered_map<std::string, MacroDef> m_macros{};
		std::vector<CallFrame> m_call_stack{};
		std::vector<std::string> m_include_stack{};
		std::unordered_map<std::string, std::unique_ptr<fe::Program>> m_include_programs{};
		std::size_t m_abort_depth{};
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
