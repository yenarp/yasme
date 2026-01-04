#ifndef YASME_FE_PARSER_HH
#define YASME_FE_PARSER_HH

#include <string>
#include <string_view>
#include <vector>
#include <yasme/fe/Ast.hh>
#include <yasme/ir/ExprParser.hh>
#include <yasme/lex/Lexer.hh>
#include <yasme/support/SourceManager.hh>
#include <yasme/support/Span.hh>

namespace yasme::fe
{
	struct ParseError
	{
		SourceSpan span{};
		std::string message{};
	};

	struct ParserOptions
	{
		bool allow_trailing_commas{true};
		bool allow_empty_data_list{false};
	};

	struct ParserResult
	{
		Program program{};
		std::vector<ParseError> errors{};

		[[nodiscard]] bool ok() const noexcept { return errors.empty(); }
	};

	class Parser
	{
	public:
		Parser(SourceManager const& sources,
			   FileId file,
			   lex::LexerOptions lex_opt = {},
			   ParserOptions opt = {});

		ParserResult parse_program();

	private:
		[[nodiscard]] bool at_end() const noexcept;
		[[nodiscard]] lex::Token const& cur() const noexcept;
		[[nodiscard]] lex::Token const& next() const noexcept;

		void advance() noexcept;
		bool accept(lex::TokenKind k) noexcept;
		lex::Token consume() noexcept;
		bool expect(lex::TokenKind k, std::string_view message);

		void skip_newlines() noexcept;
		void recover_to_newline() noexcept;
		void add_error(SourceSpan span, std::string message);

		StmtPtr parse_stmt(bool in_macro);
		StmtPtr parse_stmt_macro_def();
		StmtPtr parse_stmt_macro_call(std::string name, SourceSpan name_span);
		StmtPtr parse_stmt_local();
		StmtPtr parse_stmt_eval();
		StmtPtr parse_stmt_match();

		StmtPtr parse_stmt_org();
		StmtPtr parse_stmt_label_or_assign();
		StmtPtr parse_stmt_define();
		StmtPtr parse_stmt_emit_data(lex::TokenKind directive);
		StmtPtr parse_stmt_load();
		StmtPtr parse_stmt_virtual(bool in_macro);
		StmtPtr parse_stmt_postpone(bool in_macro);
		StmtPtr parse_stmt_unexpected_end();

		ir::Expr parse_expr(std::size_t min_prec = 0);

		[[nodiscard]] macro::TokenSlice slice_from_indices(std::size_t begin,
														   std::size_t end) const noexcept;
		[[nodiscard]] SourceSpan merge_spans(SourceSpan a, SourceSpan b) const noexcept;

	private:
		std::shared_ptr<std::vector<lex::Token>> m_tokens{};
		std::size_t m_index{};

		ParserOptions m_opt{};
		std::vector<ParseError> m_errors{};
	};

} // namespace yasme::fe

#endif /* YASME_FE_PARSER_HH */
