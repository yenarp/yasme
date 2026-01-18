#ifndef YASME_FE_AST_HH
#define YASME_FE_AST_HH

#include <memory>
#include <optional>
#include <string>
#include <variant>
#include <vector>
#include <yasme/ir/Yir.hh>
#include <yasme/lex/Tokens.hh>
#include <yasme/macro/TokenSlice.hh>
#include <yasme/support/Span.hh>

namespace yasme::fe
{
	struct Stmt;
	using StmtPtr = std::unique_ptr<Stmt>;

	enum class MacroParamKind
	{
		plain,
		name,
		ref,
		tokens,
	};

	struct MacroParam
	{
		SourceSpan span{};
		MacroParamKind kind{MacroParamKind::plain};
		std::string name{};
	};

	struct StmtMacroDef
	{
		SourceSpan span{};
		std::string name{};
		std::vector<MacroParam> params{};
		std::vector<StmtPtr> body{};
	};

	struct StmtMacroCall
	{
		SourceSpan span{};
		std::string callee{};
		std::vector<macro::TokenSlice> args{};
	};

	struct StmtLocal
	{
		SourceSpan span{};
		std::vector<std::string> names{};
	};

	struct StmtEval
	{
		SourceSpan span{};
		std::string out_name{};
		std::string tokens_name{};
	};

	struct StmtVirtual
	{
		SourceSpan span{};
		std::optional<ir::Expr> name_expr{};
		std::vector<StmtPtr> body{};
	};

	struct StmtPostpone
	{
		SourceSpan span{};
		ir::PostponeMode mode{ir::PostponeMode::at_end_each_pass};
		std::vector<StmtPtr> body{};
	};

	struct StmtMatch
	{
		SourceSpan span{};
		std::string tokens_name{};
		macro::TokenSlice pattern{};
		std::vector<StmtPtr> on_match{};
		std::vector<StmtPtr> on_else{};
		bool has_else{};
	};

	struct StmtNormal
	{
		ir::StmtPtr stmt{};
	};

	struct StmtInclude
	{
		SourceSpan span{};
		std::string raw_path{};
		FileId file{};
		std::string normalized_key{};
	};

	struct StmtIf
	{
		SourceSpan span{};
		ir::Expr cond{};
		std::vector<StmtPtr> then_body{};
		std::vector<StmtPtr> else_body{};
		bool has_else{};
	};

	struct StmtRepeat
	{
		SourceSpan span{};
		ir::Expr count{};
		std::vector<StmtPtr> body{};
	};

	struct StmtWhile
	{
		SourceSpan span{};
		ir::Expr cond{};
		std::vector<StmtPtr> body{};
	};

	struct StmtForNumeric
	{
		SourceSpan span{};
		std::string var{};
		ir::Expr start{};
		ir::Expr end{};
		std::optional<ir::Expr> step{};
		std::vector<StmtPtr> body{};
	};

	struct StmtForChars
	{
		SourceSpan span{};
		std::string var{};
		ir::Expr str{};
		std::vector<StmtPtr> body{};
	};

	enum class DiagItemKind
	{
		note,
		help,
		suggestion,
		reference,
	};

	struct DiagItem
	{
		SourceSpan span{};
		DiagItemKind kind{DiagItemKind::note};
		ir::Expr message{};
		std::optional<std::string> tokens_name{};
	};

	struct StmtMacroError
	{
		SourceSpan span{};
		ir::Expr message{};
		std::optional<std::string> primary_tokens{};
		std::vector<DiagItem> items{};
	};

	struct Stmt
	{
		using Node = std::variant<StmtMacroDef,
								  StmtMacroCall,
								  StmtLocal,
								  StmtEval,
								  StmtVirtual,
								  StmtPostpone,
								  StmtMatch,
								  StmtInclude,
								  StmtIf,
								  StmtRepeat,
								  StmtWhile,
								  StmtForNumeric,
								  StmtForChars,
								  StmtNormal,
								  StmtMacroError>;

		Node node{};

		template <class T> explicit Stmt(T v) : node(std::move(v)) {}
	};

	struct Program
	{
		std::shared_ptr<std::vector<lex::Token>> tokens{};
		std::vector<StmtPtr> stmts{};
	};

} // namespace yasme::fe

#endif /* YASME_FE_AST_HH */
