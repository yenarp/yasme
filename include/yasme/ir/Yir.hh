#ifndef YASME_IR_YIR_HH
#define YASME_IR_YIR_HH

#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>
#include <yasme/support/Span.hh>

namespace yasme::ir
{
	enum class UnaryOp
	{
		plus,
		minus,
		bit_not,
		log_not,
		at,
	};

	enum class BinaryOp
	{
		add,
		sub,
		mul,
		div,
		mod,

		shl,
		shr,

		bit_and,
		bit_or,
		bit_xor,

		log_and,
		log_or,

		eq,
		ne,
		lt,
		le,
		gt,
		ge,

		concat,
	};

	enum class BuiltinKind
	{
		dollar_address,
		stream_offset,
	};

	struct Expr;
	struct StmtMacroScope;
	struct StmtMacroReturn;

	struct ExprIdent
	{
		std::string name{};
	};

	struct ExprInt
	{
		std::int64_t value{};
	};

	struct ExprStr
	{
		std::string value{};
	};

	struct ExprBuiltin
	{
		BuiltinKind kind{};
	};

	struct ExprUnary
	{
		UnaryOp op{};
		std::unique_ptr<Expr> rhs{};
	};

	struct ExprBinary
	{
		BinaryOp op{};
		std::unique_ptr<Expr> lhs{};
		std::unique_ptr<Expr> rhs{};
	};

	struct Expr
	{
		SourceSpan span{};
		std::variant<ExprIdent, ExprInt, ExprStr, ExprBuiltin, ExprUnary, ExprBinary> node{};

		Expr() = default;

		Expr(SourceSpan s, ExprIdent v) : span(s), node(std::move(v)) {}
		Expr(SourceSpan s, ExprInt v) : span(s), node(std::move(v)) {}
		Expr(SourceSpan s, ExprStr v) : span(s), node(std::move(v)) {}
		Expr(SourceSpan s, ExprBuiltin v) : span(s), node(std::move(v)) {}
		Expr(SourceSpan s, ExprUnary v) : span(s), node(std::move(v)) {}
		Expr(SourceSpan s, ExprBinary v) : span(s), node(std::move(v)) {}
	};

	enum class DataUnit
	{
		u8,
		u16,
		u32,
		u64,
	};

	enum class PostponeMode
	{
		at_end_each_pass,
		after_stable,
	};

	struct Stmt;

	using StmtPtr = std::unique_ptr<Stmt>;
	using StmtList = std::vector<StmtPtr>;

	struct StmtOrg
	{
		SourceSpan span{};
		Expr address{};
	};

	struct StmtIf
	{
		SourceSpan span{};
		Expr cond{};
		StmtList then_body{};
		StmtList else_body{};
		bool has_else{};
	};

	struct StmtRepeat
	{
		SourceSpan span{};
		Expr count{};
		StmtList body{};
	};

	struct StmtWhile
	{
		SourceSpan span{};
		Expr cond{};
		StmtList body{};
	};

	struct StmtForNumeric
	{
		SourceSpan span{};
		std::string var{};
		Expr start{};
		Expr end{};
		std::optional<Expr> step{};
		StmtList body{};
	};

	struct StmtForChars
	{
		SourceSpan span{};
		std::string var{};
		Expr str{};
		StmtList body{};
	};

	struct StmtBreak
	{
		SourceSpan span{};
	};

	struct StmtContinue
	{
		SourceSpan span{};
	};

	enum class DiagKind
	{
		note,
		help,
		suggestion,
		reference,
	};

	struct DiagItem
	{
		SourceSpan span{};
		DiagKind kind{DiagKind::note};
		Expr message{};
		std::optional<SourceSpan> label_span{};
	};

	struct StmtError
	{
		SourceSpan span{};
		SourceSpan primary{};
		Expr message{};
		std::vector<DiagItem> items{};
	};

	struct StmtLabel
	{
		SourceSpan span{};
		std::string name{};
	};

	struct StmtAssign
	{
		SourceSpan span{};
		std::string name{};
		Expr rhs{};
	};

	struct StmtDefine
	{
		SourceSpan span{};
		std::string name{};
		Expr value{};
	};

	struct StmtEmitData
	{
		SourceSpan span{};
		DataUnit unit{};
		std::vector<Expr> items{};
	};

	struct StmtLoad
	{
		SourceSpan span{};
		DataUnit unit{};
		std::string dest{};
		Expr stream{};
		Expr offset{};
	};

	struct StmtVirtual
	{
		SourceSpan span{};
		std::optional<Expr> name_expr{};
		std::vector<StmtPtr> body{};
	};

	struct StmtPostpone
	{
		SourceSpan span{};
		PostponeMode mode{PostponeMode::at_end_each_pass};
		std::vector<StmtPtr> body{};
	};

	struct StmtMacroScope
	{
		SourceSpan span{};
		std::vector<StmtPtr> body{};
	};

	struct StmtMacroReturn
	{
		SourceSpan span{};
	};

	struct StmtEnd
	{
		SourceSpan span{};
	};

	struct Stmt
	{
		using Node = std::variant<StmtOrg,
								  StmtLabel,
								  StmtAssign,
								  StmtDefine,
								  StmtEmitData,
								  StmtLoad,
								  StmtVirtual,
								  StmtPostpone,
								  StmtIf,
								  StmtRepeat,
								  StmtWhile,
								  StmtForNumeric,
								  StmtForChars,
								  StmtBreak,
								  StmtContinue,
								  StmtError,
								  StmtMacroScope,
								  StmtMacroReturn,
								  StmtEnd>;

		Node node{};

		template <class T> explicit Stmt(T v) : node(std::move(v)) {}
	};

	struct Program
	{
		std::vector<StmtPtr> stmts{};
	};

	enum class IdentifierError
	{
		none,
		empty,
		bad_start,
		bad_char,
	};

	struct IdentifierValidation
	{
		IdentifierError error{IdentifierError::none};
		std::size_t position{};
	};

	[[nodiscard]] IdentifierValidation validate_identifier(std::string_view s) noexcept;
	[[nodiscard]] bool is_valid_identifier(std::string_view s) noexcept;

	[[nodiscard]] bool contains_space(std::string_view s) noexcept;

	[[nodiscard]] constexpr bool is_literal(Expr const& e) noexcept
	{
		return std::holds_alternative<ExprInt>(e.node) || std::holds_alternative<ExprStr>(e.node);
	}

	[[nodiscard]] constexpr bool is_name_expr(Expr const& e) noexcept
	{
		if (std::holds_alternative<ExprIdent>(e.node))
			return true;

		if (auto const* u = std::get_if<ExprUnary>(&e.node))
			return u->op == UnaryOp::at;

		if (auto const* b = std::get_if<ExprBinary>(&e.node))
			return b->op == BinaryOp::concat;

		return false;
	}

	[[nodiscard]] constexpr bool is_builtin(Expr const& e) noexcept
	{
		return std::holds_alternative<ExprBuiltin>(e.node);
	}

	[[nodiscard]] std::string_view builtin_kind_spelling(BuiltinKind k) noexcept;

	[[nodiscard]] std::string_view unary_op_spelling(UnaryOp op) noexcept;
	[[nodiscard]] std::string_view binary_op_spelling(BinaryOp op) noexcept;

} // namespace yasme::ir

#endif /* YASME_IR_YIR_HH */
