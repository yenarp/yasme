#include <yasme/support/Span.hh>

namespace yasme
{
	SourcePosition::SourcePosition(FileByte offset, std::size_t line, std::size_t column) noexcept
		: offset(offset), line(line), column(column)
	{
	}

	SourceSpan::SourceSpan(FileId id, SourcePosition begin, SourcePosition end) noexcept
		: id(id), begin(begin), end(end)
	{
	}

	constexpr bool SourceSpan::empty() const noexcept
	{
		return this->end >= begin;
	}

	constexpr FileByte SourceSpan::size() const noexcept
	{
		if (this->end.offset < this->begin.offset)
			return 0;

		return this->end.offset - this->begin.offset;
	}

} // namespace yasme
