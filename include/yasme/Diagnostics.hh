#ifndef YASME_DIAGNOSTICS_HH
#define YASME_DIAGNOSTICS_HH

#include <cstddef>
#include <iosfwd>
#include <string>
#include <vector>
#include <yasme/support/SourceManager.hh>
#include <yasme/support/Span.hh>

namespace yasme
{
	enum class DiagnosticLevel
	{
		note,
		warning,
		error,
	};

	enum class ColorMode
	{
		auto_detect,
		always,
		never,
	};

	enum class AdviceKind
	{
		note,
		help,
		suggestion,
	};

	struct DiagnosticAdvice
	{
		AdviceKind kind{AdviceKind::note};
		std::string message{};
	};

	struct DiagnosticFixIt
	{
		SourceSpan span{};
		std::string replacement{};
		std::string message{};
	};

	enum class LabelKind
	{
		primary_like,
		note,
		help,
		suggestion,
		reference,
	};

	struct DiagnosticLabel
	{
		std::string message{};
		SourceSpan span{};
		LabelKind kind{LabelKind::note};
	};

	struct Diagnostic
	{
		DiagnosticLevel level{DiagnosticLevel::error};

		std::string message{};
		SourceSpan primary{};

		std::vector<DiagnosticLabel> labels{};
		std::vector<DiagnosticAdvice> advices{};

		std::vector<DiagnosticFixIt> fixits{};
	};

	class Diagnostics
	{
	public:
		explicit Diagnostics(SourceManager const& sources, std::ostream& out) noexcept;

		void set_color_mode(ColorMode mode) noexcept;
		[[nodiscard]] ColorMode color_mode() const noexcept;

		void emit(Diagnostic const& diag);

		[[nodiscard]] std::size_t error_count() const noexcept;
		[[nodiscard]] std::size_t warning_count() const noexcept;

	private:
		void emit_header(Diagnostic const& diag);

		[[nodiscard]] bool use_color() const noexcept;

		SourceManager const* m_sources{};
		std::ostream* m_out{};

		ColorMode m_color_mode{ColorMode::auto_detect};

		std::size_t m_errors{};
		std::size_t m_warnings{};

		bool m_need_separator{};
		bool m_has_last_anchor{};
		SourceSpan m_last_anchor{};
		DiagnosticLevel m_last_anchor_level{DiagnosticLevel::note};
	};

} // namespace yasme

#endif /* YASME_DIAGNOSTICS_HH */
