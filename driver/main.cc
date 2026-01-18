#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <filesystem>
#include <iostream>
#include <limits>
#include <optional>
#include <span>
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
	struct CliOptions
	{
		std::string input{};
		std::string output{};

		std::vector<std::string> inline_lines{};

		std::size_t max_passes{100};
		bool error_on_unresolved{true};
		bool run_final_postpone{true};
		std::vector<std::string> include_paths{};

		yasme::ColorMode color{yasme::ColorMode::auto_detect};
		bool show_help{};
		bool show_version{};
	};

	static void ensure_newline(std::string& s)
	{
		if (!s.empty() && s.back() == '\n')
			return;

		s.push_back('\n');
	}

	[[nodiscard]] std::string_view strip_surrounding_quotes(std::string_view s) noexcept
	{
		if (s.size() < 2)
			return s;

		auto const q0 = s.front();
		auto const q1 = s.back();
		if ((q0 == '"' && q1 == '"') || (q0 == '\'' && q1 == '\''))
			return s.substr(1, s.size() - 2);

		return s;
	}

	[[nodiscard]] std::string_view trim(std::string_view s) noexcept
	{
		while (!s.empty()
			   && (s.front() == ' ' || s.front() == '\t' || s.front() == '\n' || s.front() == '\r'))
			s.remove_prefix(1);

		while (!s.empty()
			   && (s.back() == ' ' || s.back() == '\t' || s.back() == '\n' || s.back() == '\r'))
			s.remove_suffix(1);

		return s;
	}

	[[nodiscard]] std::vector<std::string> split_path_list(std::string_view s)
	{
		std::vector<std::string> out{};
		if (s.empty())
			return out;

		s = strip_surrounding_quotes(trim(s));

#if defined(_WIN32)
		auto const sep = ';';
#else
		auto const sep = ':';
#endif

		std::size_t i = 0;
		while (i <= s.size())
		{
			auto j = s.find(sep, i);
			if (j == std::string_view::npos)
				j = s.size();

			auto part = trim(s.substr(i, j - i));
			part = strip_surrounding_quotes(trim(part));
			if (!part.empty())
				out.push_back(std::string(part));

			if (j == s.size())
				break;

			i = j + 1;
		}

		return out;
	}

	[[nodiscard]] std::vector<std::string> read_auto_include_paths()
	{
		auto* e = std::getenv("YASME_INCLUDE");
		if (e == nullptr)
			return {};

		return split_path_list(std::string_view(e));
	}

	[[nodiscard]] std::string_view exe_basename(std::string_view p) noexcept
	{
		auto const pos = p.find_last_of("/\\");
		if (pos == std::string_view::npos)
			return p;
		return p.substr(pos + 1);
	}

	void print_help(std::ostream& os, std::string_view exe)
	{
		os << "usage: " << exe << " [options] <input>\n\n"
		   << "options:\n"
		   << "  -o, --output <file>        output file\n"
		   << "  -i <line>                  inline assembly statement before input (repeatable)\n"
		   << "  -I, --include <dir>        add include search path (repeatable)\n"
		   << "      --max-passes <n>       maximum assembly passes\n"
		   << "      --no-error-unresolved  do not error on unresolved symbols in final pass\n"
		   << "      --no-final-postpone    do not run 'postpone !' finalization blocks\n"
		   << "      --color <auto|always|never>\n"
		   << "  -h, --help                 show this help\n"
		   << "      --version              show version\n";
	}

	void print_version(std::ostream& os, std::string_view exe)
	{
		os << exe << " (yasme) version 0.0.1\n";
	}

	[[nodiscard]] bool parse_u64(std::string_view s, std::uint64_t& out) noexcept
	{
		if (s.empty())
			return false;

		std::uint64_t v = 0;
		for (char ch : s)
		{
			if (ch < '0' || ch > '9')
				return false;

			auto const digit = static_cast<std::uint64_t>(ch - '0');

			if (v > (std::numeric_limits<std::uint64_t>::max() - digit) / 10)
				return false;

			v = v * 10 + digit;
		}

		out = v;
		return true;
	}

	[[nodiscard]] std::optional<std::string_view> take_value(int& i, int argc, char** argv)
	{
		if (i + 1 >= argc)
			return std::nullopt;

		++i;
		return std::string_view(argv[i]);
	}

	[[nodiscard]] std::optional<std::string> take_inline_line(int& i, int argc, char** argv)
	{
		auto v = take_value(i, argc, argv);
		if (!v)
			return std::nullopt;

		if (!v->empty() && v->front() == '-')
			return std::nullopt;

		std::string_view token = *v;
		std::string cleaned{};
		cleaned.reserve(token.size());
		for (char ch : token)
		{
			if (ch == '"')
				continue;
			cleaned.push_back(ch);
		}

		return cleaned;
	}

	[[nodiscard]] std::optional<CliOptions> parse_args(int argc, char** argv)
	{
		CliOptions opt{};

		for (int i = 1; i < argc; ++i)
		{
			std::string_view a(argv[i]);

			if (a == "-h" || a == "--help")
			{
				opt.show_help = true;
				return opt;
			}

			if (a == "--version")
			{
				opt.show_version = true;
				return opt;
			}

			if (a == "-o" || a == "--output")
			{
				auto v = take_value(i, argc, argv);
				if (!v)
					return std::nullopt;

				opt.output = std::string(*v);
				continue;
			}

			if (a == "--max-passes")
			{
				auto v = take_value(i, argc, argv);
				if (!v)
					return std::nullopt;

				std::uint64_t n = 0;
				if (!parse_u64(*v, n))
					return std::nullopt;

				opt.max_passes = static_cast<std::size_t>(n);
				continue;
			}

			if (a == "-i")
			{
				auto v = take_inline_line(i, argc, argv);
				if (!v)
					return std::nullopt;

				opt.inline_lines.push_back(*v);
				continue;
			}

			if ((a == "-I" || a == "--include") && i + 1 < argc)
			{
				opt.include_paths.push_back(argv[++i]);
				continue;
			}

			if (a.rfind("-I", 0) == 0 && a.size() > 2)
			{
				opt.include_paths.push_back(std::string{a.substr(2)});
				continue;
			}

			if (a == "--no-error-unresolved")
			{
				opt.error_on_unresolved = false;
				continue;
			}

			if (a == "--no-final-postpone")
			{
				opt.run_final_postpone = false;
				continue;
			}

			if (a == "--color")
			{
				auto v = take_value(i, argc, argv);
				if (!v)
					return std::nullopt;

				if (*v == "auto")
					opt.color = yasme::ColorMode::auto_detect;
				else if (*v == "always")
					opt.color = yasme::ColorMode::always;
				else if (*v == "never")
					opt.color = yasme::ColorMode::never;
				else
					return std::nullopt;

				continue;
			}

			if (!a.empty() && a.front() == '-')
				return std::nullopt;

			if (opt.input.empty())
				opt.input = std::string(a);
			else
				return std::nullopt;
		}

		if (opt.input.empty() && !opt.show_help && !opt.show_version)
			return std::nullopt;

		return opt;
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

} // namespace

int main(int argc, char** argv)
{
	auto const exe =
		exe_basename((argc > 0) ? std::string_view(argv[0]) : std::string_view("yasme"));

	auto parsed = parse_args(argc, argv);
	if (!parsed)
	{
		print_help(std::cerr, exe);
		return 1;
	}

	auto opt = *parsed;

	if (opt.output.empty())
	{
		auto out = opt.input;

		if (out.size() >= 4 && out.ends_with(".asm"))
			out.resize(out.size() - 4);

		opt.output = out;
	}

	if (opt.show_help)
	{
		print_help(std::cout, exe);
		return 0;
	}

	if (opt.show_version)
	{
		print_version(std::cout, exe);
		return 0;
	}

	auto auto_includes = read_auto_include_paths();

	std::vector<std::string> final_includes{};
	final_includes.reserve(opt.include_paths.size() + auto_includes.size());

	for (auto const& p : opt.include_paths)
		final_includes.push_back(p);

	for (auto const& p : auto_includes)
		final_includes.push_back(p);

	yasme::SourceManager sources{};
	yasme::Diagnostics diag(sources, std::cerr);
	diag.set_color_mode(opt.color);

	auto in_id_res = sources.open_read(opt.input);
	if (!in_id_res.ok())
	{
		std::cerr << exe << ": error: failed to open input '" << opt.input
				  << "': " << in_id_res.err() << "\n";
		return 1;
	}
	auto const in_id = in_id_res.value();

	yasme::fe::ParserOptions fe_opt{};
	fe_opt.include_paths = final_includes;

	yasme::fe::Program combined{};
	if (!opt.inline_lines.empty())
	{
		yasme::fe::ParserOptions inline_fe_opt = fe_opt;
		{
			namespace fs = std::filesystem;
			fs::path in_path(sources.name(in_id));
			auto in_dir = in_path.parent_path();
			if (!in_dir.empty())
			{
				auto dir = in_dir.string();
				auto& v = inline_fe_opt.include_paths;
				if (std::find(v.begin(), v.end(), dir) == v.end())
					v.insert(v.begin(), std::move(dir));
			}
		}

		std::string src{};
		for (auto const& line : opt.inline_lines)
		{
			src.append(line);
			ensure_newline(src);
		}

		auto inline_id = sources.add_virtual("<inline>", std::move(src));
		yasme::fe::Parser inline_parser(sources, inline_id, {}, inline_fe_opt);
		auto inline_res = inline_parser.parse_program();

		if (!inline_res.errors.empty())
		{
			emit_parse_errors(diag, inline_res.errors);
			return 1;
		}

		for (auto& st : inline_res.program.stmts)
			combined.stmts.push_back(std::move(st));
	}

	yasme::fe::Parser parser(sources, in_id, {}, fe_opt);
	auto parse_res = parser.parse_program();

	if (!parse_res.errors.empty())
	{
		emit_parse_errors(diag, parse_res.errors);
		return 1;
	}

	for (auto& st : parse_res.program.stmts)
		combined.stmts.push_back(std::move(st));

	yasme::macro::Expander expander(sources, diag);
	auto ir_program = expander.expand(combined);

	if (diag.error_count() != 0)
		return 1;

	yasme::Assembler assembler(diag);

	yasme::AssembleOptions asm_opt{};
	asm_opt.max_passes = opt.max_passes;
	asm_opt.error_on_unresolved = opt.error_on_unresolved;
	asm_opt.run_final_postpone = opt.run_final_postpone;

	auto out = assembler.assemble(ir_program, asm_opt);

	if (diag.error_count() != 0 || out.errors != 0)
		return 1;

	auto out_id_res = sources.open_write(opt.output);
	if (!out_id_res.ok())
	{
		std::cerr << exe << ": error: failed to open output '" << opt.output
				  << "': " << out_id_res.err() << "\n";
		return 1;
	}
	auto const out_id = out_id_res.value();

	auto* w = sources.writer(out_id);
	if (w == nullptr)
	{
		std::cerr << exe << ": error: internal: output file handle not available\n";
		return 1;
	}

	const auto bytes = std::span<std::uint8_t>(out.bytes.data(), out.bytes.size());
	w->write(bytes);

	return 0;
}
