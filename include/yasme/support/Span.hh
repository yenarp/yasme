#ifndef YASME_SUPPORT_SPAN_HH
#define YASME_SUPPORT_SPAN_HH

#include <compare>
#include <cstddef>

namespace yasme
{
	using FileId = std::size_t;
	using FileByte = std::size_t;

	struct SourcePosition
	{
		FileByte offset = 0;

		std::size_t line = 1;
		std::size_t column = 1;

		constexpr SourcePosition() = default;
		SourcePosition(FileByte offset, std::size_t line, std::size_t column) noexcept;

		constexpr bool operator==(const SourcePosition&) const = default;
		std::strong_ordering operator<=>(const SourcePosition&) const = default;
	};

	struct SourceSpan
	{
		FileId id = 0;

		SourcePosition begin{};
		SourcePosition end{};

		SourceSpan() noexcept = default;
		SourceSpan(FileId id, SourcePosition begin, SourcePosition end) noexcept;

		[[nodiscard]] constexpr bool empty() const noexcept;
		[[nodiscard]] constexpr FileByte size() const noexcept;
	};

} // namespace yasme

#endif /* YASME_SUPPORT_SPAN_HH */
