#include <algorithm>
#include <iostream>
#include <ostream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <yasme/Diagnostics.hh>

#if defined(_WIN32)
#	include <io.h>
#	define YASME_ISATTY _isatty
#	define YASME_FILENO _fileno
#else
#	include <unistd.h>
#	define YASME_ISATTY isatty
#	define YASME_FILENO fileno
#endif

namespace yasme
{
	namespace
	{
		struct Style
		{
			std::string_view reset{"\x1b[0m"};
			std::string_view bold{"\x1b[1m"};
			std::string_view dim{"\x1b[2m"};

			std::string_view red{"\x1b[31m"};
			std::string_view yellow{"\x1b[33m"};
			std::string_view cyan{"\x1b[36m"};
			std::string_view green{"\x1b[32m"};
		};

		[[nodiscard]] std::string_view level_text(DiagnosticLevel lvl) noexcept
		{
			switch (lvl)
			{
				case DiagnosticLevel::note:
					return "note";
				case DiagnosticLevel::warning:
					return "warning";
				case DiagnosticLevel::error:
					return "error";
			}
			return "error";
		}

		[[nodiscard]] std::string_view level_color(DiagnosticLevel lvl) noexcept
		{
			switch (lvl)
			{
				case DiagnosticLevel::note:
					return "\x1b[36m";
				case DiagnosticLevel::warning:
					return "\x1b[33m";
				case DiagnosticLevel::error:
					return "\x1b[31m";
			}
			return "\x1b[31m";
		}

		[[nodiscard]] std::string_view advice_text(AdviceKind k) noexcept
		{
			switch (k)
			{
				case AdviceKind::note:
					return "note";
				case AdviceKind::help:
					return "help";
				case AdviceKind::suggestion:
					return "suggestion";
			}
			return "note";
		}

		[[nodiscard]] std::string_view advice_color(AdviceKind k) noexcept
		{
			switch (k)
			{
				case AdviceKind::note:
					return "\x1b[36m";
				case AdviceKind::help:
					return "\x1b[32m";
				case AdviceKind::suggestion:
					return "\x1b[32m";
			}
			return "\x1b[36m";
		}

		[[nodiscard]] bool is_tty(std::ostream& out) noexcept
		{
			if (out.rdbuf() == std::cout.rdbuf())
				return YASME_ISATTY(YASME_FILENO(stdout)) != 0;

			if (out.rdbuf() == std::cerr.rdbuf())
				return YASME_ISATTY(YASME_FILENO(stderr)) != 0;

			return false;
		}

		[[nodiscard]] std::size_t digits(std::size_t v) noexcept
		{
			std::size_t d = 1;
			while (v >= 10)
			{
				v /= 10;
				++d;
			}
			return d;
		}

		[[nodiscard]] std::string_view rstrip_newline(std::string_view s) noexcept
		{
			while (!s.empty() && (s.back() == '\n' || s.back() == '\r'))
				s.remove_suffix(1);

			return s;
		}

		void write_repeat(std::ostream& out, char ch, std::size_t n)
		{
			for (std::size_t i = 0; i < n; ++i)
				out.put(ch);
		}

		[[nodiscard]] std::size_t clamp_col(std::size_t col) noexcept
		{
			return col == 0 ? 1 : col;
		}

		[[nodiscard]] std::size_t
		visual_column(std::string_view line, std::size_t col_1_based, std::size_t tabstop) noexcept
		{
			if (col_1_based <= 1)
				return 1;

			auto visual = std::size_t{1};
			auto i = std::size_t{1};

			for (auto ch : line)
			{
				if (i >= col_1_based)
					break;

				if (ch == '\t')
				{
					auto rem = (visual - 1) % tabstop;
					visual += tabstop - rem;
				}
				else
				{
					++visual;
				}

				++i;
			}

			return visual;
		}

		[[nodiscard]] std::string expand_tabs(std::string_view line, std::size_t tabstop)
		{
			std::string out{};
			out.reserve(line.size());

			auto visual = std::size_t{1};
			for (auto ch : line)
			{
				if (ch == '\t')
				{
					auto rem = (visual - 1) % tabstop;
					auto spaces = tabstop - rem;
					out.append(spaces, ' ');
					visual += spaces;
					continue;
				}

				out.push_back(ch);
				++visual;
			}

			return out;
		}

		[[nodiscard]] std::string_view ltrim_ws(std::string_view s) noexcept
		{
			while (!s.empty() && (s.front() == ' ' || s.front() == '\t'))
				s.remove_prefix(1);
			return s;
		}

		[[nodiscard]] bool starts_with(std::string_view s, std::string_view pref) noexcept
		{
			return s.size() >= pref.size() && s.substr(0, pref.size()) == pref;
		}

		[[nodiscard]] std::string_view strip_redundant_level_prefix(std::string_view msg) noexcept
		{
			for (;;)
			{
				auto s = ltrim_ws(msg);

				auto try_strip = [&](std::string_view lvl) -> bool {
					if (!starts_with(s, lvl))
						return false;
					if (s.size() <= lvl.size() || s[lvl.size()] != ':')
						return false;

					s.remove_prefix(lvl.size() + 1);
					msg = ltrim_ws(s);
					return true;
				};

				if (try_strip("note") || try_strip("warning") || try_strip("error"))
					continue;

				return ltrim_ws(msg);
			}
		}

		[[nodiscard]] bool same_span(SourceSpan const& a, SourceSpan const& b) noexcept
		{
			return a.id == b.id && a.begin.line == b.begin.line && a.begin.column == b.begin.column
				   && a.end.line == b.end.line && a.end.column == b.end.column;
		}

		[[nodiscard]] bool same_single_line_span(SourceSpan const& a, SourceSpan const& b) noexcept
		{
			return a.id == b.id && a.begin.line == b.begin.line && a.end.line == b.end.line
				   && a.begin.column == b.begin.column && a.end.column == b.end.column;
		}

		struct Marker
		{
			SourceSpan span{};
			std::string message{};
			bool is_primary{};
			LabelKind kind{LabelKind::note};
			bool suppressed{};
		};

		struct LineGroup
		{
			std::size_t line{};
			std::string_view raw{};
			std::string expanded{};
			std::vector<Marker> markers{};
		};

		struct FileGroup
		{
			decltype(SourceSpan{}.id) id{};
			std::vector<LineGroup> lines{};
			std::size_t anchor_line{1};
			std::size_t anchor_col{1};
			bool is_primary_file{};
		};

	} // namespace

	Diagnostics::Diagnostics(SourceManager const& sources, std::ostream& out) noexcept
		: m_sources(&sources), m_out(&out)
	{
	}

	void Diagnostics::set_color_mode(ColorMode mode) noexcept
	{
		m_color_mode = mode;
	}

	ColorMode Diagnostics::color_mode() const noexcept
	{
		return m_color_mode;
	}

	std::size_t Diagnostics::error_count() const noexcept
	{
		return m_errors;
	}

	std::size_t Diagnostics::warning_count() const noexcept
	{
		return m_warnings;
	}

	bool Diagnostics::use_color() const noexcept
	{
		if (!m_out)
			return false;

		switch (m_color_mode)
		{
			case ColorMode::always:
				return true;
			case ColorMode::never:
				return false;
			case ColorMode::auto_detect:
				return is_tty(*m_out);
		}
		return false;
	}

	void Diagnostics::emit_header(Diagnostic const& diag)
	{
		auto& out = *m_out;
		Style st{};

		auto const& sp = diag.primary;
		auto name = m_sources->name(sp.id);

		auto line = sp.begin.line;
		auto col = sp.begin.column;

		auto const msg = strip_redundant_level_prefix(diag.message);

		if (use_color())
		{
			out << st.bold << name << st.reset << ':' << line << ':' << col << ": " << st.bold
				<< level_color(diag.level) << level_text(diag.level) << st.reset << ": " << msg
				<< '\n';
		}
		else
		{
			out << name << ':' << line << ':' << col << ": " << level_text(diag.level) << ": "
				<< msg << '\n';
		}
	}

	void Diagnostics::emit(Diagnostic const& diag)
	{
		if (!m_out || !m_sources)
			return;

		auto& out = *m_out;
		Style st{};

		auto const is_attached_note = (diag.level == DiagnosticLevel::note) && m_has_last_anchor
									  && same_span(diag.primary, m_last_anchor)
									  && (m_last_anchor_level == DiagnosticLevel::error
										  || m_last_anchor_level == DiagnosticLevel::warning);

		if (is_attached_note)
		{
			auto const msg = strip_redundant_level_prefix(diag.message);

			if (use_color())
			{
				out << "  " << st.dim << "=" << st.reset << ' ' << st.bold
					<< advice_color(AdviceKind::note) << "note" << st.reset << ": " << msg << '\n';
			}
			else
			{
				out << "  = note: " << msg << '\n';
			}

			for (auto const& a : diag.advices)
			{
				auto const amsg = strip_redundant_level_prefix(a.message);
				if (use_color())
				{
					out << "  " << st.dim << "=" << st.reset << ' ' << st.bold
						<< advice_color(a.kind) << advice_text(a.kind) << st.reset << ": " << amsg
						<< '\n';
				}
				else
				{
					out << "  = " << advice_text(a.kind) << ": " << amsg << '\n';
				}
			}

			return;
		}

		if (m_need_separator)
			out << '\n';
		m_need_separator = true;

		switch (diag.level)
		{
			case DiagnosticLevel::error:
				++m_errors;
				break;
			case DiagnosticLevel::warning:
				++m_warnings;
				break;
			case DiagnosticLevel::note:
				break;
		}

		emit_header(diag);

		auto const tabstop = std::size_t{4};

		using FileId = decltype(diag.primary.id);

		std::vector<FileGroup> files{};

		auto file_group = [&](FileId id) -> FileGroup& {
			for (auto& f : files)
				if (f.id == id)
					return f;

			FileGroup f{};
			f.id = id;
			f.is_primary_file = (id == diag.primary.id);
			files.push_back(std::move(f));
			return files.back();
		};

		auto line_group = [&](FileGroup& fg, std::size_t line) -> LineGroup& {
			for (auto& lg : fg.lines)
				if (lg.line == line)
					return lg;

			LineGroup lg{};
			lg.line = line;

			auto raw = m_sources->line_view(fg.id, line);
			raw = rstrip_newline(raw);

			lg.raw = raw;
			if (!raw.empty())
				lg.expanded = expand_tabs(raw, tabstop);

			fg.lines.push_back(std::move(lg));
			return fg.lines.back();
		};

		auto remember_anchor = [&](FileGroup& fg, SourceSpan const& sp) {
			if (sp.id == 0)
				return;

			auto const line = sp.begin.line == 0 ? 1 : sp.begin.line;
			auto const col = sp.begin.column == 0 ? 1 : sp.begin.column;

			if (fg.anchor_line == 1 && fg.anchor_col == 1 && fg.lines.empty())
			{
				fg.anchor_line = line;
				fg.anchor_col = col;
				return;
			}

			if (line < fg.anchor_line || (line == fg.anchor_line && col < fg.anchor_col))
			{
				fg.anchor_line = line;
				fg.anchor_col = col;
			}
		};

		std::vector<Marker> markers{};

		markers.push_back(Marker{.span = diag.primary,
								 .message = {},
								 .is_primary = true,
								 .kind = LabelKind::primary_like,
								 .suppressed = false});

		for (auto const& l : diag.labels)
		{
			markers.push_back(Marker{
				.span = l.span,
				.message = l.message,
				.is_primary = false,
				.kind = l.kind,
				.suppressed = false,
			});
		}

		{
			std::vector<Marker> uniq{};
			for (auto& m : markers)
			{
				auto dup = false;
				for (auto const& u : uniq)
				{
					if (same_span(m.span, u.span) && m.message == u.message
						&& m.is_primary == u.is_primary)
					{
						dup = true;
						break;
					}
				}
				if (!dup)
					uniq.push_back(std::move(m));
			}
			markers = std::move(uniq);
		}

		std::vector<std::string> multi_line_notes{};

		for (auto& m : markers)
		{
			if (m.span.id == 0)
				continue;

			auto& fg = file_group(m.span.id);
			remember_anchor(fg, m.span);

			if (m.span.begin.line == m.span.end.line)
			{
				auto& lg = line_group(fg, m.span.begin.line);
				lg.markers.push_back(std::move(m));
			}
			else
			{
				std::string_view msg = m.message.empty() ? std::string_view{}
														 : strip_redundant_level_prefix(m.message);

				auto note = std::string{};
				if (!msg.empty())
				{
					note += ": ";
					note += msg;
				}
				multi_line_notes.push_back(std::move(note));
			}
		}

		for (auto& fg : files)
		{
			for (auto& lg : fg.lines)
			{
				auto* primary = static_cast<Marker*>(nullptr);
				for (auto& m : lg.markers)
				{
					if (m.is_primary)
					{
						primary = &m;
						break;
					}
				}

				if (primary && primary->message.empty())
				{
					for (auto& m : lg.markers)
					{
						if (m.is_primary || m.suppressed || m.message.empty())
							continue;

						if (same_single_line_span(m.span, primary->span))
						{
							primary->message = std::move(m.message);
							m.suppressed = true;
							break;
						}
					}
				}

				for (std::size_t i = 0; i < lg.markers.size(); ++i)
				{
					if (lg.markers[i].suppressed)
						continue;

					for (std::size_t j = i + 1; j < lg.markers.size(); ++j)
					{
						if (lg.markers[j].suppressed)
							continue;

						if (!same_single_line_span(lg.markers[i].span, lg.markers[j].span))
							continue;

						auto a = strip_redundant_level_prefix(lg.markers[i].message);
						auto b = strip_redundant_level_prefix(lg.markers[j].message);

						if (a.empty() && !b.empty())
						{
							lg.markers[i].message = std::string{b};
							lg.markers[j].suppressed = true;
							continue;
						}
						if (!a.empty() && !b.empty() && a != b)
						{
							lg.markers[i].message = std::string{a} + "; " + std::string{b};
							lg.markers[j].suppressed = true;
						}
					}
				}

				std::stable_sort(
					lg.markers.begin(), lg.markers.end(), [&](Marker const& a, Marker const& b) {
						if (a.suppressed != b.suppressed)
							return a.suppressed < b.suppressed;
						if (a.is_primary != b.is_primary)
							return a.is_primary > b.is_primary;
						return clamp_col(a.span.begin.column) < clamp_col(b.span.begin.column);
					});
			}

			std::sort(fg.lines.begin(), fg.lines.end(), [](LineGroup const& a, LineGroup const& b) {
				return a.line < b.line;
			});
		}

		std::stable_sort(files.begin(), files.end(), [&](FileGroup const& a, FileGroup const& b) {
			if (a.is_primary_file != b.is_primary_file)
				return a.is_primary_file > b.is_primary_file;
			return m_sources->name(a.id) < m_sources->name(b.id);
		});

		auto render_file_block = [&](FileGroup const& fg) {
			auto const name = m_sources->name(fg.id);

			if (!fg.is_primary_file)
			{
				if (use_color())
				{
					out << st.dim << "  --> " << st.reset << name << ':' << fg.anchor_line << ':'
						<< fg.anchor_col << '\n';
				}
				else
				{
					out << "  --> " << name << ':' << fg.anchor_line << ':' << fg.anchor_col
						<< '\n';
				}
			}

			auto max_line = std::size_t{1};
			for (auto const& lg : fg.lines)
				max_line = std::max(max_line, lg.line);

			auto const width = digits(max_line);

			auto print_bar = [&] {
				write_repeat(out, ' ', width + 1);
				if (use_color())
					out << st.dim;

				out << "|";
				if (use_color())
					out << st.reset;

				out << '\n';
			};

			auto print_elision = [&] {
				write_repeat(out, ' ', width + 1);
				if (use_color())
					out << st.dim;

				out << "| ...";
				if (use_color())
					out << st.reset;

				out << '\n';
			};

			auto print_source = [&](std::size_t line_no, std::string_view expanded) {
				out << ' ' << line_no;
				write_repeat(out, ' ', (width - digits(line_no)) + 1);
				if (use_color())
					out << st.dim;

				out << "|";
				if (use_color())
					out << st.reset;

				out << ' ' << expanded << '\n';
			};

			auto print_marker_prefix = [&] {
				write_repeat(out, ' ', width + 1);
				if (use_color())
					out << st.dim;

				out << "|";
				if (use_color())
					out << st.reset;

				out << ' ';
			};

			auto print_marker = [&](LineGroup const& lg, Marker const& m) {
				auto const raw = lg.raw;

				auto start_col = clamp_col(m.span.begin.column);
				auto end_col = clamp_col(m.span.end.column);
				if (end_col < start_col)
					std::swap(end_col, start_col);

				auto const visual_start = visual_column(raw, start_col, tabstop);
				auto const len = end_col > start_col ? (end_col - start_col) : 1;
				auto const visual_end = visual_column(raw, start_col + len, tabstop);
				auto const visual_len = visual_end > visual_start ? (visual_end - visual_start) : 1;

				print_marker_prefix();
				write_repeat(out, ' ', visual_start);

				if (use_color())
					out << st.bold << level_color(diag.level);

				out << '^';
				if (visual_len > 1)
					write_repeat(out, '~', visual_len - 1);

				if (use_color())
					out << st.reset;

				auto const msg = strip_redundant_level_prefix(m.message);
				if (!msg.empty())
				{
					out << ' ';
					if (use_color())
						out << st.bold;

					out << msg;
					if (use_color())
						out << st.reset;
				}

				out << '\n';
			};

			print_bar();

			auto prev_line = std::size_t{0};
			for (auto const& lg : fg.lines)
			{
				if (prev_line != 0 && lg.line > prev_line + 1)
					print_elision();

				if (!lg.raw.empty())
				{
					print_source(lg.line, lg.expanded);

					for (auto const& m : lg.markers)
					{
						if (m.suppressed)
							continue;

						print_marker(lg, m);
					}
				}

				prev_line = lg.line;
			}
		};

		for (auto const& fg : files)
			render_file_block(fg);

		for (auto const& note : multi_line_notes)
		{
			if (use_color())
			{
				out << "  " << st.dim << "=" << st.reset << ' ' << st.bold
					<< advice_color(AdviceKind::note) << "note" << st.reset << ": " << note << '\n';
			}
			else
			{
				out << "  = note: " << note << '\n';
			}
		}

		for (auto const& a : diag.advices)
		{
			auto const msg = strip_redundant_level_prefix(a.message);
			if (msg.empty())
				continue;

			if (use_color())
			{
				out << "  " << st.dim << "=" << st.reset << ' ' << st.bold << advice_color(a.kind)
					<< advice_text(a.kind) << st.reset << ": " << msg << '\n';
			}
			else
			{
				out << "  = " << advice_text(a.kind) << ": " << msg << '\n';
			}
		}

		for (auto const& fx : diag.fixits)
		{
			auto const name = fx.span.id != 0 ? m_sources->name(fx.span.id) : std::string_view{};
			auto const line = fx.span.begin.line;
			auto const col = fx.span.begin.column;

			auto msg = strip_redundant_level_prefix(fx.message);
			if (msg.empty())
				msg = "apply the suggested fix";

			if (use_color())
			{
				out << "  " << st.dim << "=" << st.reset << ' ' << st.bold
					<< advice_color(AdviceKind::help) << "help" << st.reset << ": " << msg;

				if (fx.span.id != 0)
					out << " (" << name << ':' << line << ':' << col << ')';
				out << '\n';
			}
			else
			{
				out << "  = help: " << msg;
				if (fx.span.id != 0)
					out << " (" << name << ':' << line << ':' << col << ')';
				out << '\n';
			}

			if (fx.span.id == 0 || fx.span.begin.line == 0
				|| fx.span.begin.line != fx.span.end.line)
				continue;

			auto raw = m_sources->line_view(fx.span.id, fx.span.begin.line);
			raw = rstrip_newline(raw);
			if (raw.empty())
				continue;

			auto const start_col = clamp_col(fx.span.begin.column);
			auto const end_col = clamp_col(fx.span.end.column);

			auto start_idx = start_col > 0 ? (start_col - 1) : 0;
			auto end_idx = end_col > 0 ? (end_col - 1) : 0;

			if (end_idx < start_idx)
				std::swap(end_idx, start_idx);

			if (start_idx > raw.size())
				start_idx = raw.size();

			if (end_idx > raw.size())
				end_idx = raw.size();

			auto replaced = std::string{};
			replaced.reserve(raw.size() + fx.replacement.size());

			replaced.append(raw.substr(0, start_idx));
			replaced.append(fx.replacement);
			replaced.append(raw.substr(end_idx));

			out << "  |\n";
			out << "  | - " << raw << '\n';
			out << "  | + " << replaced << '\n';
		}

		if (diag.level == DiagnosticLevel::error || diag.level == DiagnosticLevel::warning)
		{
			m_last_anchor = diag.primary;
			m_has_last_anchor = true;
			m_last_anchor_level = diag.level;
		}
	}

} // namespace yasme
