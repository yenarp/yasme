#ifndef YASME_IR_PARSER_RESULT_HH
#define YASME_IR_PARSER_RESULT_HH

#include <string>
#include <vector>
#include <yasme/ir/Yir.hh>
#include <yasme/support/Span.hh>

namespace yasme::ir
{
	struct ParseError
	{
		SourceSpan span{};
		std::string message{};
	};

	struct ParserResult
	{
		Program program{};
		std::vector<ParseError> errors{};

		[[nodiscard]] bool ok() const noexcept { return errors.empty(); }
	};

} // namespace yasme::ir

#endif /* YASME_IR_PARSER_RESULT_HH */
