#ifndef YASME_SUPPORT_SOURCE_MANAGER_HH
#define YASME_SUPPORT_SOURCE_MANAGER_HH

#include <fstream>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>
#include <yasme/support/Result.hh>
#include <yasme/support/Span.hh>

namespace yasme
{
	template <class SourceFileSpecifier> class SourceFile
	{
	public:
		SourceFile() = default;

		[[nodiscard]] bool is_open() const noexcept { return m_handle.is_open(); }
		void close() noexcept { m_handle.close(); }

		void set_id(FileId new_id) noexcept { m_id = new_id; }
		[[nodiscard]] FileId file_id() const noexcept { return m_id; }

	protected:
		template <class... Args> void init(this auto&& self, Args&&... args) noexcept
		{
			self.m_offset = 0;
			self.open_impl(std::forward<Args>(args)...);
		}

		std::fstream m_handle{};
		FileId m_id{};
		FileByte m_offset{};
	};

	class ReadSourceFile : public SourceFile<ReadSourceFile>
	{
	public:
		ReadSourceFile() = default;

		explicit ReadSourceFile(std::string_view filename) noexcept;

		void open_impl(std::string_view filename) noexcept;

		[[nodiscard]] bool eof() const noexcept;
		[[nodiscard]] SourcePosition position() const noexcept;
		[[nodiscard]] SourceSpan span(SourcePosition begin, SourcePosition end) const noexcept;
		[[nodiscard]] SourceSpan span_from(SourcePosition begin) const noexcept;

		SourceSpan read(std::span<char>& dst) noexcept;

	private:
		void advance_for_bytes(std::span<const char> bytes) noexcept;

		std::size_t m_current_line{};
		std::size_t m_current_column{};
	};

	class WriteSourceFile : public SourceFile<WriteSourceFile>
	{
	public:
		WriteSourceFile() = default;

		explicit WriteSourceFile(std::string_view filename) noexcept;

		void open_impl(std::string_view filename) noexcept;

		[[nodiscard]] FileByte position() const noexcept;

		FileByte write(std::string_view src) noexcept;
		FileByte write(const std::span<std::uint8_t> src) noexcept;
		FileByte write(const std::uint8_t* src, std::size_t len) noexcept;
	};

	class SourceManager
	{
	public:
		SourceManager() = default;

		SourceManager(SourceManager const&) = delete;
		SourceManager& operator=(SourceManager const&) = delete;

		SourceManager(SourceManager&&) noexcept = default;
		SourceManager& operator=(SourceManager&&) noexcept = default;

		~SourceManager();

		Result<FileId, std::string> open_read(std::string_view filename) noexcept;
		Result<FileId, std::string> open_write(std::string_view filename) noexcept;

		FileId add_virtual(std::string name, std::string contents) noexcept;

		void close(FileId id) noexcept;

		[[nodiscard]] bool has(FileId id) const noexcept;

		[[nodiscard]] std::string_view name(FileId id) const noexcept;

		[[nodiscard]] ReadSourceFile* reader(FileId id) noexcept;
		[[nodiscard]] ReadSourceFile const* reader(FileId id) const noexcept;

		[[nodiscard]] WriteSourceFile* writer(FileId id) noexcept;
		[[nodiscard]] WriteSourceFile const* writer(FileId id) const noexcept;

		[[nodiscard]] std::string_view content(FileId id) const noexcept;

		[[nodiscard]] std::size_t line_count(FileId id) const noexcept;
		[[nodiscard]] std::string_view line_view(FileId id,
												 std::size_t line_1_based) const noexcept;

		[[nodiscard]] SourcePosition position_from_offset(FileId id,
														  FileByte offset) const noexcept;
		[[nodiscard]] SourceSpan
		span_from_offsets(FileId id, FileByte begin, FileByte end) const noexcept;

	private:
		struct Entry
		{
			FileId id{};
			std::string name{};

			std::unique_ptr<ReadSourceFile> read{};
			std::unique_ptr<WriteSourceFile> write{};

			std::string buffer{};
			std::vector<FileByte> line_starts{};
		};

		[[nodiscard]] Entry* get(FileId id) noexcept;
		[[nodiscard]] Entry const* get(FileId id) const noexcept;

		static void rebuild_line_index(Entry& e) noexcept;

		FileId m_next_id{1};
		std::unordered_map<FileId, Entry> m_entries{};
	};

} // namespace yasme

#endif /* YASME_SUPPORT_SOURCE_MANAGER_HH */
