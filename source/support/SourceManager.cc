#include <yasme/support/SourceManager.hh>
#include <yasme/support/Span.hh>

namespace yasme
{
	ReadSourceFile::ReadSourceFile(std::string_view filename) noexcept
	{
		init(filename);
		m_current_line = 1;
		m_current_column = 1;
	}

	void ReadSourceFile::open_impl(std::string_view filename) noexcept
	{
		m_handle.open(filename.data(), std::ios_base::in | std::ios_base::binary);
	}

	bool ReadSourceFile::eof() const noexcept
	{
		return !m_handle.good() || m_handle.eof();
	}

	SourcePosition ReadSourceFile::position() const noexcept
	{
		return SourcePosition{m_offset, m_current_line, m_current_column};
	}

	SourceSpan ReadSourceFile::span(SourcePosition begin, SourcePosition end) const noexcept
	{
		return SourceSpan{file_id(), begin, end};
	}

	SourceSpan ReadSourceFile::span_from(SourcePosition begin) const noexcept
	{
		return span(begin, position());
	}

	void ReadSourceFile::advance_for_bytes(std::span<const char> bytes) noexcept
	{
		for (char c : bytes)
		{
			++m_offset;

			if (c == '\n')
			{
				++m_current_line;
				m_current_column = 1;
				continue;
			}

			++m_current_column;
		}
	}

	SourceSpan ReadSourceFile::read(std::span<char>& dst) noexcept
	{
		const SourcePosition begin = position();

		if (!m_handle.good() || dst.empty())
			return span(begin, begin);

		m_handle.read(dst.data(), static_cast<std::streamsize>(dst.size()));
		const auto got = static_cast<std::size_t>(m_handle.gcount());

		advance_for_bytes(std::span<const char>{dst.data(), got});

		const SourcePosition end = position();
		return span(begin, end);
	}

	WriteSourceFile::WriteSourceFile(std::string_view filename) noexcept
	{
		init(filename);
	}

	void WriteSourceFile::open_impl(std::string_view filename) noexcept
	{
		m_handle.open(filename.data(),
					  std::ios_base::out | std::ios_base::trunc | std::ios_base::binary);
	}

	FileByte WriteSourceFile::position() const noexcept
	{
		return m_offset;
	}

	FileByte WriteSourceFile::write(std::string_view src) noexcept
	{
		const FileByte begin = position();

		if (!m_handle.good() || src.empty())
			return begin;

		m_handle.write(src.data(), static_cast<std::streamsize>(src.size()));

		if (!m_handle.good())
			return begin;

		const FileByte end = m_offset = begin + src.size();

		return end;
	}

	FileByte WriteSourceFile::write(const std::span<std::uint8_t> src) noexcept
	{
		const FileByte begin = position();

		if (!m_handle.good() || src.empty())
			return begin;

		m_handle.write(reinterpret_cast<char*>(src.data()),
					   static_cast<std::streamsize>(src.size()));

		if (!m_handle.good())
			return begin;

		const FileByte end = m_offset = begin + src.size();

		return end;
	}

	FileByte WriteSourceFile::write(const std::uint8_t* src, std::size_t len) noexcept
	{
		const FileByte begin = position();

		if (!m_handle.good() || len == 0 || !src)
			return begin;

		m_handle.write(reinterpret_cast<const char*>(src), static_cast<std::streamsize>(len));

		if (!m_handle.good())
			return begin;

		const FileByte end = m_offset = begin + len;

		return end;
	}

	SourceManager::~SourceManager()
	{
		for (auto& [_, e] : m_entries)
		{
			if (e.read)
				e.read->close();

			if (e.write)
				e.write->close();
		}
	}

	auto SourceManager::get(FileId id) noexcept -> Entry*
	{
		auto it = m_entries.find(id);
		if (it == m_entries.end())
			return nullptr;

		return &it->second;
	}

	auto SourceManager::get(FileId id) const noexcept -> Entry const*
	{
		auto it = m_entries.find(id);
		if (it == m_entries.end())
			return nullptr;

		return &it->second;
	}

	void SourceManager::rebuild_line_index(Entry& e) noexcept
	{
		e.line_starts.clear();
		e.line_starts.push_back(0);

		for (FileByte i = 0; i < e.buffer.size(); ++i)
			if (e.buffer[i] == '\n')
				e.line_starts.push_back(i + 1);
	}

	Result<FileId, std::string> SourceManager::open_read(std::string_view filename) noexcept
	{
		auto entry = Entry{};
		entry.id = m_next_id++;
		entry.name = std::string(filename);

		auto rf = std::make_unique<ReadSourceFile>(filename);
		rf->set_id(entry.id);

		if (!rf->is_open())
			return std::string("failed to open file for reading");

		{
			std::ifstream in(std::string(filename), std::ios::binary);
			if (in)
			{
				entry.buffer.assign(std::istreambuf_iterator<char>(in),
									std::istreambuf_iterator<char>());
				rebuild_line_index(entry);
			}
		}

		entry.read = std::move(rf);
		m_entries.emplace(entry.id, std::move(entry));
		return entry.id;
	}

	Result<FileId, std::string> SourceManager::open_write(std::string_view filename) noexcept
	{
		auto entry = Entry{};
		entry.id = m_next_id++;
		entry.name = std::string(filename);

		auto wf = std::make_unique<WriteSourceFile>(filename);
		wf->set_id(entry.id);

		if (!wf->is_open())
			return std::string("failed to open file for writing");

		entry.write = std::move(wf);
		m_entries.emplace(entry.id, std::move(entry));
		return entry.id;
	}

	FileId SourceManager::add_virtual(std::string name, std::string contents) noexcept
	{
		auto entry = Entry{};
		entry.id = m_next_id++;
		entry.name = std::move(name);
		entry.buffer = std::move(contents);
		rebuild_line_index(entry);

		auto id = entry.id;
		m_entries.emplace(id, std::move(entry));
		return id;
	}

	void SourceManager::close(FileId id) noexcept
	{
		auto* e = get(id);
		if (!e)
			return;

		if (e->read)
			e->read->close();

		if (e->write)
			e->write->close();
	}

	bool SourceManager::has(FileId id) const noexcept
	{
		return get(id) != nullptr;
	}

	std::string_view SourceManager::name(FileId id) const noexcept
	{
		auto const* e = get(id);
		if (!e)
			return {};

		return e->name;
	}

	ReadSourceFile* SourceManager::reader(FileId id) noexcept
	{
		auto* e = get(id);
		return e && e->read ? e->read.get() : nullptr;
	}

	ReadSourceFile const* SourceManager::reader(FileId id) const noexcept
	{
		auto const* e = get(id);
		return e && e->read ? e->read.get() : nullptr;
	}

	WriteSourceFile* SourceManager::writer(FileId id) noexcept
	{
		auto* e = get(id);
		return e && e->write ? e->write.get() : nullptr;
	}

	WriteSourceFile const* SourceManager::writer(FileId id) const noexcept
	{
		auto const* e = get(id);
		return e && e->write ? e->write.get() : nullptr;
	}

	std::string_view SourceManager::content(FileId id) const noexcept
	{
		auto const* e = get(id);
		if (!e)
			return {};

		return e->buffer;
	}

	std::size_t SourceManager::line_count(FileId id) const noexcept
	{
		auto const* e = get(id);
		if (!e)
			return 0;

		if (e->buffer.empty())
			return 0;

		return e->line_starts.size();
	}

	std::string_view SourceManager::line_view(FileId id, std::size_t line_1_based) const noexcept
	{
		auto const* e = get(id);
		if (!e || e->buffer.empty())
			return {};

		if (line_1_based == 0)
			return {};

		auto idx = line_1_based - 1;
		if (idx >= e->line_starts.size())
			return {};

		auto begin = e->line_starts[idx];

		auto end = static_cast<FileByte>(e->buffer.size());
		if (idx + 1 < e->line_starts.size())
			end = e->line_starts[idx + 1];

		while (end > begin && (e->buffer[end - 1] == '\n' || e->buffer[end - 1] == '\r'))
			--end;

		return std::string_view(e->buffer.data() + begin, end - begin);
	}

	SourcePosition SourceManager::position_from_offset(FileId id, FileByte offset) const noexcept
	{
		auto const* e = get(id);
		if (!e)
			return {};

		if (e->line_starts.empty())
			return SourcePosition{offset, 1, offset + 1};

		auto off = std::min<FileByte>(offset, e->buffer.size());

		auto it = std::upper_bound(e->line_starts.begin(), e->line_starts.end(), off);
		auto line_idx = static_cast<std::size_t>(it - e->line_starts.begin());

		if (line_idx == 0)
			line_idx = 1;

		auto line_1_based = line_idx;
		auto line_start = e->line_starts[line_idx - 1];
		auto col_1_based = static_cast<std::size_t>((off - line_start) + 1);

		return SourcePosition{off, line_1_based, col_1_based};
	}

	SourceSpan
	SourceManager::span_from_offsets(FileId id, FileByte begin, FileByte end) const noexcept
	{
		auto b = position_from_offset(id, begin);
		auto e = position_from_offset(id, end);
		return SourceSpan{id, b, e};
	}

} // namespace yasme
