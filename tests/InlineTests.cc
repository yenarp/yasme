#include <algorithm>
#include <cstdint>
#include <iostream>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>
#include <yasme/Diagnostics.hh>
#include <yasme/asm/Assembler.hh>
#include <yasme/fe/Parser.hh>
#include <yasme/macro/Expander.hh>
#include <yasme/support/SourceManager.hh>

namespace
{
	bool check(bool cond, std::string_view msg)
	{
		if (cond)
			return true;

		std::cerr << "FAIL: " << msg << "\n";
		return false;
	}

	void emit_parse_errors(yasme::Diagnostics& diag, std::span<const yasme::fe::ParseError> errs)
	{
		for (auto const& e : errs)
		{
			yasme::Diagnostic d{};
			d.level = yasme::DiagnosticLevel::error;
			d.message = e.message;
			d.primary = e.span;
			diag.emit(d);
		}
	}

	struct AssembleResult
	{
		std::vector<std::uint8_t> bytes{};
		std::string diag_text{};
		std::size_t diag_errors{};
		std::size_t asm_errors{};
	};

	AssembleResult assemble_text(std::string_view main_text, std::vector<std::string> inline_lines)
	{
		yasme::SourceManager sources{};
		std::ostringstream diag_out{};
		yasme::Diagnostics diag(sources, diag_out);
		diag.set_color_mode(yasme::ColorMode::never);

		auto main_id = sources.add_virtual("<inline-tests-input>", std::string(main_text));

		yasme::fe::Parser parser(sources, main_id);
		auto parse_res = parser.parse_program();
		if (!parse_res.errors.empty())
			emit_parse_errors(diag, parse_res.errors);

		std::vector<std::uint8_t> out_bytes{};
		std::size_t asm_errors = 0;

		if (diag.error_count() == 0)
		{
			yasme::macro::Expander expander(sources, diag);
			auto ir_program = expander.expand(parse_res.program);

			if (diag.error_count() == 0)
			{
				yasme::Assembler assembler(sources, diag);

				yasme::AssembleOptions opt{};
				opt.inline_lines = std::move(inline_lines);

				auto out = assembler.assemble(ir_program, opt);
				out_bytes = std::move(out.bytes);
				asm_errors = out.errors;
			}
		}

		AssembleResult r{};
		r.bytes = std::move(out_bytes);
		r.diag_text = diag_out.str();
		r.diag_errors = diag.error_count();
		r.asm_errors = asm_errors;
		return r;
	}

	bool bytes_eq(std::vector<std::uint8_t> const& a, std::vector<std::uint8_t> const& b)
	{
		return a.size() == b.size() && std::equal(a.begin(), a.end(), b.begin());
	}

} // namespace

int main()
{
	int failures = 0;

	{
		auto r = assemble_text("start:\n"
							   "db start\n",
							   {
								   "define base, 16",
								   "org base",
							   });

		failures += !check(r.diag_errors == 0, "inline: no diagnostics for define/org");
		failures += !check(r.asm_errors == 0, "inline: no assembler errors for define/org");
		failures += !check(bytes_eq(r.bytes, {16}), "inline: label address reflects inline org");
	}

	{
		auto r = assemble_text("db 2\n",
							   {
								   "db 1",
							   });

		failures += !check(r.diag_errors == 0, "inline emit: no diagnostics");
		failures += !check(r.asm_errors == 0, "inline emit: no assembler errors");
		failures += !check(bytes_eq(r.bytes, {1, 2}), "inline emit: bytes are prefixed");
	}

	{
		auto r = assemble_text("db 0\n",
							   {
								   "define , 1",
							   });

		failures += !check(r.diag_errors != 0, "inline parse error: diagnostics reported");
		failures +=
			!check(r.diag_text.find("expected identifier after 'define'") != std::string::npos,
				   "inline parse error: message contains expected identifier after 'define'");
	}

	if (failures != 0)
		std::cerr << failures << " test(s) failed\n";

	return failures == 0 ? 0 : 1;
}
