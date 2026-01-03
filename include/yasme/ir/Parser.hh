#ifndef YASME_IR_PARSER_HH
#define YASME_IR_PARSER_HH

#include <cstddef>
#include <optional>
#include <string>
#include <string_view>
#include <vector>
#include <yasme/ir/ParserResult.hh>
#include <yasme/ir/Yir.hh>
#include <yasme/lex/Lexer.hh>
#include <yasme/lex/Tokens.hh>
#include <yasme/support/SourceManager.hh>

namespace yasme::ir
{
	struct ParserOptions
	{
		bool allow_trailing_commas{true};
		bool allow_empty_data_list{false};
	};

	class Parser
	{
	public:
		explicit Parser(SourceManager const& sources,
						FileId file,
						lex::LexerOptions lex_opt = {},
						ParserOptions opt = {}) noexcept;

		[[nodiscard]] FileId file_id() const noexcept { return m_lex.file_id(); }

		ParserResult parse_program();

	private:
		[[nodiscard]] bool at_end() const noexcept;

		void advance() noexcept;
		[[nodiscard]] lex::Token const& cur() const noexcept { return m_cur; }
		[[nodiscard]] lex::Token const& next_tok() const noexcept { return m_next; }

		[[nodiscard]] bool is(lex::TokenKind k) const noexcept;
		[[nodiscard]] bool is_next(lex::TokenKind k) const noexcept;

		bool accept(lex::TokenKind k) noexcept;
		lex::Token consume() noexcept;

		bool expect(lex::TokenKind k, std::string_view message);
		void skip_newlines() noexcept;

		void add_error(SourceSpan span, std::string message);
		void recover_to_newline() noexcept;

		StmtPtr parse_stmt();
		StmtPtr parse_stmt_org();
		StmtPtr parse_stmt_label_or_assign();
		StmtPtr parse_stmt_define();
		StmtPtr parse_stmt_emit_data(lex::TokenKind directive);
		StmtPtr parse_stmt_virtual();
		StmtPtr parse_stmt_postpone();
		StmtPtr parse_stmt_unexpected_end();

		Expr parse_expr(std::size_t min_prec = 0);
		Expr parse_unary();
		Expr parse_primary();

		[[nodiscard]] std::optional<BinaryOp> token_to_binary_op(lex::TokenKind k) const noexcept;
		[[nodiscard]] std::size_t precedence(BinaryOp op) const noexcept;
		[[nodiscard]] bool is_right_associative(BinaryOp op) const noexcept;

		[[nodiscard]] Expr make_ident(lex::Token const& t) const;
		[[nodiscard]] Expr make_int(lex::Token const& t) const;
		[[nodiscard]] Expr make_str(lex::Token const& t) const;
		[[nodiscard]] Expr make_builtin(SourceSpan s, BuiltinKind k) const;
		[[nodiscard]] Expr make_unary(SourceSpan s, UnaryOp op, Expr rhs) const;
		[[nodiscard]] Expr make_binary(SourceSpan s, BinaryOp op, Expr lhs, Expr rhs) const;

		static SourceSpan merge_spans(SourceSpan a, SourceSpan b) noexcept;

		static std::string unquote(std::string_view s);

	private:
		lex::Lexer m_lex;
		ParserOptions m_opt{};

		lex::Token m_cur{};
		lex::Token m_next{};

		std::vector<ParseError> m_errors{};
	};

} // namespace yasme::ir

#endif /* YASME_IR_PARSER_HH */
