#include <yasme/ir/Yir.hh>
#include <yasme/support/Ctype.hh>

namespace yasme::ir
{
	IdentifierValidation validate_identifier(std::string_view s) noexcept
	{
		if (s.empty())
			return {IdentifierError::empty, 0};

		if (!is_ident_start(s.front()))
			return {IdentifierError::bad_start, 0};

		for (std::size_t i = 1; i < s.size(); ++i)
		{
			if (!is_ident_continue(s[i]))
				return {IdentifierError::bad_char, i};
		}

		return {IdentifierError::none, 0};
	}

	bool is_valid_identifier(std::string_view s) noexcept
	{
		return validate_identifier(s).error == IdentifierError::none;
	}

	bool contains_space(std::string_view s) noexcept
	{
		for (char c : s)
			if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\v' || c == '\f')
				return true;

		return false;
	}

	std::string_view builtin_kind_spelling(BuiltinKind k) noexcept
	{
		switch (k)
		{
			case BuiltinKind::dollar_address:
				return "$";
			case BuiltinKind::stream_offset:
				return "@$";
		}
		return "?";
	}

	std::string_view unary_op_spelling(UnaryOp op) noexcept
	{
		switch (op)
		{
			case UnaryOp::plus:
				return "+";
			case UnaryOp::minus:
				return "-";
			case UnaryOp::bit_not:
				return "~";
			case UnaryOp::log_not:
				return "!";
			case UnaryOp::at:
				return "@";
		}
		return "?";
	}

	std::string_view binary_op_spelling(BinaryOp op) noexcept
	{
		switch (op)
		{
			case BinaryOp::add:
				return "+";
			case BinaryOp::sub:
				return "-";
			case BinaryOp::mul:
				return "*";
			case BinaryOp::div:
				return "/";
			case BinaryOp::mod:
				return "%";

			case BinaryOp::shl:
				return "<<";
			case BinaryOp::shr:
				return ">>";

			case BinaryOp::bit_and:
				return "&";
			case BinaryOp::bit_or:
				return "|";
			case BinaryOp::bit_xor:
				return "^";

			case BinaryOp::log_and:
				return "&&";
			case BinaryOp::log_or:
				return "||";

			case BinaryOp::eq:
				return "==";
			case BinaryOp::ne:
				return "!=";
			case BinaryOp::lt:
				return "<";
			case BinaryOp::le:
				return "<=";
			case BinaryOp::gt:
				return ">";
			case BinaryOp::ge:
				return ">=";

			case BinaryOp::concat:
				return "#";
		}
		return "?";
	}

} // namespace yasme::ir
