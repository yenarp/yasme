#include <cstddef>
#include <iostream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <yasme/Diagnostics.hh>
#include <yasme/support/SourceManager.hh>
#include <yasme/support/Span.hh>

namespace
{
	using yasme::AdviceKind;
	using yasme::ColorMode;
	using yasme::Diagnostic;
	using yasme::DiagnosticAdvice;
	using yasme::DiagnosticFixIt;
	using yasme::DiagnosticLabel;
	using yasme::DiagnosticLevel;
	using yasme::Diagnostics;
	using yasme::FileByte;
	using yasme::FileId;
	using yasme::LabelKind;
	using yasme::SourceManager;
	using yasme::SourceSpan;

	SourceSpan
	span_nth(SourceManager const& sm, FileId id, std::string_view needle, std::size_t nth = 0)
	{
		auto const hay = sm.content(id);

		std::size_t pos{};
		for (std::size_t i{}; i <= nth; ++i)
		{
			pos = hay.find(needle, pos);
			if (pos == std::string_view::npos)
				return sm.span_from_offsets(id, 0, 0);

			if (i != nth)
				pos += needle.size();
		}

		return sm.span_from_offsets(
			id, static_cast<FileByte>(pos), static_cast<FileByte>(pos + needle.size()));
	}

	SourceSpan span_after(SourceManager const& sm,
						  FileId id,
						  std::string_view anchor,
						  std::string_view needle,
						  std::size_t nth_after_anchor = 0)
	{
		auto const hay = sm.content(id);
		auto const a = hay.find(anchor);
		if (a == std::string_view::npos)
			return sm.span_from_offsets(id, 0, 0);

		std::size_t pos = a;
		for (std::size_t i{}; i <= nth_after_anchor; ++i)
		{
			pos = hay.find(needle, pos);
			if (pos == std::string_view::npos)
				return sm.span_from_offsets(id, 0, 0);

			if (i != nth_after_anchor)
				pos += needle.size();
		}

		return sm.span_from_offsets(
			id, static_cast<FileByte>(pos), static_cast<FileByte>(pos + needle.size()));
	}

	void emit(Diagnostics& diags,
			  DiagnosticLevel lvl,
			  std::string msg,
			  SourceSpan primary,
			  std::vector<DiagnosticLabel> labels = {},
			  std::vector<DiagnosticAdvice> advices = {},
			  std::vector<DiagnosticFixIt> fixits = {})
	{
		Diagnostic d{};
		d.level = lvl;
		d.message = std::move(msg);
		d.primary = primary;
		d.labels = std::move(labels);
		d.advices = std::move(advices);
		d.fixits = std::move(fixits);
		diags.emit(d);
	}

} // namespace

int main()
{
	SourceManager sm;

	auto const main_src = std::string{".intel_syntax noprefix\n"
									  ".text\n"
									  ".global _start\n"
									  "_start:\n"
									  "    mvo eax, ebx\n"
									  "    mov eaz, ebx\n"
									  "    add eax\n"
									  "    mov eax, byte ptr [rbx]\n"
									  "    mov eax, [rbx + rcx*3]\n"
									  "    mov al, 300\n"
									  "    jmp missing_label\n"
									  "    mov eax, eax\n"
									  ".loop:\n"
									  "    dec ecx\n"
									  "    jnz loop\n"
									  ".loop:\n"
									  "    ret\n"};

	auto const a_src = std::string{"global_func:\n"
								   "    ret\n"};

	auto const b_src = std::string{"global_func:\n"
								   "    nop\n"
								   "    ret\n"};

	auto const main_id = sm.add_virtual("main.s", main_src);
	auto const a_id = sm.add_virtual("a.s", a_src);
	auto const b_id = sm.add_virtual("b.s", b_src);

	Diagnostics diags(sm, std::cout);
	diags.set_color_mode(ColorMode::auto_detect);

	emit(diags,
		 DiagnosticLevel::error,
		 "unknown instruction 'mvo'",
		 span_nth(sm, main_id, "mvo"),
		 {
			 DiagnosticLabel{.message = "did you mean 'mov'?",
							 .span = span_nth(sm, main_id, "mvo"),
							 .kind = LabelKind::primary_like},
		 },
		 {},
		 {
			 DiagnosticFixIt{.span = span_nth(sm, main_id, "mvo"),
							 .replacement = "mov",
							 .message = "replace with the valid instruction"},
		 });

	emit(diags,
		 DiagnosticLevel::error,
		 "invalid src operand 'eaz'",
		 span_nth(sm, main_id, "eaz"),
		 {
			 DiagnosticLabel{.message = "invalid src operand",
							 .span = span_nth(sm, main_id, "eaz"),
							 .kind = LabelKind::help},
		 },
		 {},
		 {
			 DiagnosticFixIt{.span = span_nth(sm, main_id, "eaz"),
							 .replacement = "eax",
							 .message = "did you mean 'eax'?"},
		 });

	emit(diags,
		 DiagnosticLevel::error,
		 "missing operand: 'add' expects 2 operands",
		 span_nth(sm, main_id, "add eax"),
		 {
			 DiagnosticLabel{.message = "example: add eax, 1",
							 .span = span_nth(sm, main_id, "add eax"),
							 .kind = LabelKind::help},
		 });

	auto const size_mismatch_line = std::string_view{"mov eax, byte ptr [rbx]"};
	emit(diags,
		 DiagnosticLevel::error,
		 "operand size mismatch: cannot move 8-bit memory into 32-bit register",
		 span_after(sm, main_id, size_mismatch_line, "byte ptr [rbx]"),
		 {
			 DiagnosticLabel{.message = "destination is 32-bit (eax)",
							 .span = span_after(sm, main_id, size_mismatch_line, "eax"),
							 .kind = LabelKind::note},
			 DiagnosticLabel{.message = "source is 8-bit (byte ptr ...)",
							 .span = span_after(sm, main_id, size_mismatch_line, "byte ptr [rbx]"),
							 .kind = LabelKind::note},
		 });

	auto const scale_line = std::string_view{"mov eax, [rbx + rcx*3]"};
	emit(diags,
		 DiagnosticLevel::error,
		 "invalid index scale 3 (must be 1, 2, 4, or 8)",
		 span_after(sm, main_id, scale_line, "*3"),
		 {
			 DiagnosticLabel{.message = "address expression here",
							 .span = span_after(sm, main_id, scale_line, "[rbx + rcx*3]"),
							 .kind = LabelKind::reference},
		 },
		 {
			 DiagnosticAdvice{.kind = AdviceKind::help,
							  .message = "use rcx*1, rcx*2, rcx*4, or rcx*8"},
		 });

	auto const al_line = std::string_view{"mov al, 300"};
	emit(diags,
		 DiagnosticLevel::error,
		 "immediate out of range for 'al' (expected 0..255)",
		 span_after(sm, main_id, al_line, "300"),
		 {
			 DiagnosticLabel{.message = "al is 8-bit",
							 .span = span_after(sm, main_id, al_line, "al"),
							 .kind = LabelKind::help},
		 },
		 {},
		 {
			 DiagnosticFixIt{.span = span_after(sm, main_id, al_line, "300"),
							 .replacement = "255",
							 .message = "use an 8-bit immediate"},
		 });

	emit(diags,
		 DiagnosticLevel::error,
		 "undefined symbol 'missing_label'",
		 span_nth(sm, main_id, "missing_label"),
		 {
			 DiagnosticLabel{.message = "referenced by this jump",
							 .span = span_nth(sm, main_id, "jmp missing_label"),
							 .kind = LabelKind::reference},
		 },
		 {
			 DiagnosticAdvice{.kind = AdviceKind::help,
							  .message = "define the label like `missing_label:`"},
		 });

	emit(diags,
		 DiagnosticLevel::note,
		 "define the label like `missing_label:`",
		 span_nth(sm, main_id, "missing_label"));

	emit(diags,
		 DiagnosticLevel::warning,
		 "redundant instruction: 'mov eax, eax' has no effect",
		 span_nth(sm, main_id, "mov eax, eax"),
		 {
			 DiagnosticLabel{.message = "remove this instruction",
							 .span = span_nth(sm, main_id, "mov eax, eax"),
							 .kind = LabelKind::suggestion},
		 },
		 {},
		 {
			 DiagnosticFixIt{.span = span_nth(sm, main_id, "mov eax, eax"),
							 .replacement = "",
							 .message = "delete this instruction"},
		 });

	emit(diags,
		 DiagnosticLevel::error,
		 "duplicate label 'loop'",
		 span_nth(sm, main_id, ".loop:", 1),
		 {
			 DiagnosticLabel{.message = "previous definition here",
							 .span = span_nth(sm, main_id, ".loop:", 0),
							 .kind = LabelKind::reference},
		 });

	emit(diags,
		 DiagnosticLevel::error,
		 "multiple definition of symbol 'global_func'",
		 span_nth(sm, b_id, "global_func"),
		 {
			 DiagnosticLabel{.message = "first defined here",
							 .span = span_nth(sm, a_id, "global_func"),
							 .kind = LabelKind::reference},
		 });

	auto const expected_errors = std::size_t{9};
	auto const expected_warnings = std::size_t{1};

	auto const ok =
		(diags.error_count() == expected_errors) && (diags.warning_count() == expected_warnings);

	std::cout << "\n--- summary ---\n";
	std::cout << "errors:   " << diags.error_count() << " (expected " << expected_errors << ")\n";
	std::cout << "warnings: " << diags.warning_count() << " (expected " << expected_warnings
			  << ")\n";
	std::cout << (ok ? "DIAGNOSTICS TESTS OK\n" : "DIAGNOSTICS TESTS FAILED\n");

	return ok ? 0 : 1;
}
