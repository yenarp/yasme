#ifndef YASME_MACRO_EXPANDER_HH
#define YASME_MACRO_EXPANDER_HH

#include <yasme/fe/Ast.hh>
#include <yasme/ir/Yir.hh>

namespace yasme
{
	class Diagnostics;
	class SourceManager;

} // namespace yasme

namespace yasme::macro
{
	class Expander
	{
	public:
		explicit Expander(SourceManager const& sources, Diagnostics& diag) noexcept;

		[[nodiscard]] ir::Program expand(fe::Program const& program);

	private:
		SourceManager const* m_sources{};
		Diagnostics* m_diag{};
	};

} // namespace yasme::macro

#endif /* YASME_MACRO_EXPANDER_HH */
