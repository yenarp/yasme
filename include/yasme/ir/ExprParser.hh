#ifndef YASME_IR_EXPR_PARSER_HH
#define YASME_IR_EXPR_PARSER_HH

#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <yasme/ir/Yir.hh>
#include <yasme/lex/Tokens.hh>
#include <yasme/support/Span.hh>

namespace yasme
{
	class Diagnostics;
	class SourceManager;

} // namespace yasme

namespace yasme::macro
{
	struct TokenSlice;

} // namespace yasme::macro

namespace yasme::ir
{
	class ExprParser
	{
	public:
		using CurFn = std::function<lex::Token const&()>;
		using AdvanceFn = std::function<void()>;
		using ErrorFn = std::function<void(SourceSpan, std::string)>;

		ExprParser(CurFn cur, AdvanceFn advance, ErrorFn error);

		Expr parse_expr(std::size_t min_prec = 0);

	private:
		[[nodiscard]] bool is(lex::TokenKind k) const;
		bool accept(lex::TokenKind k);
		lex::Token consume();

		bool expect(lex::TokenKind k, std::string_view message);

		[[nodiscard]] std::optional<BinaryOp> token_to_binary_op(lex::TokenKind k) const noexcept;
		[[nodiscard]] std::size_t precedence(BinaryOp op) const noexcept;
		[[nodiscard]] bool is_right_associative(BinaryOp op) const noexcept;

		Expr parse_unary();
		Expr parse_primary();

		[[nodiscard]] Expr make_ident(lex::Token const& t) const;
		[[nodiscard]] Expr make_int(lex::Token const& t) const;
		[[nodiscard]] Expr make_str(lex::Token const& t) const;
		[[nodiscard]] Expr make_builtin(SourceSpan s, BuiltinKind k) const;
		[[nodiscard]] Expr make_unary(SourceSpan s, UnaryOp op, Expr rhs) const;
		[[nodiscard]] Expr make_binary(SourceSpan s, BinaryOp op, Expr lhs, Expr rhs) const;

		static SourceSpan merge_spans(SourceSpan a, SourceSpan b) noexcept;
		static std::string unquote(std::string_view s);

	private:
		CurFn m_cur;
		AdvanceFn m_advance;
		ErrorFn m_error;
	};

	Expr parse_expr_from_tokens(SourceManager const& sources,
								FileId file,
								macro::TokenSlice slice,
								Diagnostics& diag);

} // namespace yasme::ir

#endif /* YASME_IR_EXPR_PARSER_HH */
