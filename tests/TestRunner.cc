#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>
#include <yasme/Diagnostics.hh>
#include <yasme/asm/Assembler.hh>
#include <yasme/ir/Parser.hh>
#include <yasme/support/SourceManager.hh>

namespace
{
	[[nodiscard]] std::string read_text_file(std::filesystem::path const& p)
	{
		std::ifstream f(p, std::ios::binary);
		if (!f)
			return {};

		std::string s;
		f.seekg(0, std::ios::end);
		auto const size = f.tellg();
		if (size > 0)
			s.resize(static_cast<std::size_t>(size));

		f.seekg(0, std::ios::beg);
		if (!s.empty())
			f.read(s.data(), static_cast<std::streamsize>(s.size()));

		return s;
	}

	[[nodiscard]] std::vector<std::uint8_t> read_bin_file(std::filesystem::path const& p)
	{
		std::ifstream f(p, std::ios::binary);
		if (!f)
			return {};

		std::string buf;
		f.seekg(0, std::ios::end);
		auto const size = f.tellg();
		if (size > 0)
			buf.resize(static_cast<std::size_t>(size));

		f.seekg(0, std::ios::beg);
		if (!buf.empty())
			f.read(buf.data(), static_cast<std::streamsize>(buf.size()));

		std::vector<std::uint8_t> out;
		out.reserve(buf.size());
		for (unsigned char ch : buf)
			out.push_back(static_cast<std::uint8_t>(ch));

		return out;
	}

	[[nodiscard]] constexpr bool is_hex_digit(char c) noexcept
	{
		auto const uc = static_cast<unsigned char>(c);
		auto const lc = static_cast<char>(std::tolower(uc));
		return (c >= '0' && c <= '9') || (lc >= 'a' && lc <= 'f');
	}

	[[nodiscard]] std::optional<unsigned> hex_value(char c) noexcept
	{
		if (c >= '0' && c <= '9')
			return static_cast<unsigned>(c - '0');
		if (c >= 'a' && c <= 'f')
			return static_cast<unsigned>(c - 'a') + 10;
		if (c >= 'A' && c <= 'F')
			return static_cast<unsigned>(c - 'A') + 10;
		return std::nullopt;
	}

	[[nodiscard]] std::optional<unsigned> parse_token_as_byte(std::string_view tok) noexcept
	{
		if (tok.empty())
			return std::nullopt;

		auto base = 16;
		auto start = std::size_t{0};
		auto end = tok.size();

		if (tok.size() >= 2 && tok[0] == '0' && (tok[1] == 'x' || tok[1] == 'X'))
		{
			base = 16;
			start = 2;
		}
		else if (tok.size() >= 2 && tok[0] == '0' && (tok[1] == 'd' || tok[1] == 'D'))
		{
			base = 10;
			start = 2;
		}
		else
		{
			if (end >= 2 && (tok[end - 1] == 'd' || tok[end - 1] == 'D'))
			{
				bool digits_only = true;
				for (std::size_t i = 0; i + 1 < end; ++i)
				{
					auto const c = tok[i];
					if (c < '0' || c > '9')
					{
						digits_only = false;
						break;
					}
				}

				if (digits_only)
				{
					base = 10;
					--end;
				}
			}
		}

		unsigned v = 0;

		if (start >= end)
			return std::nullopt;

		for (std::size_t i = start; i < end; ++i)
		{
			auto const c = tok[i];

			unsigned digit = 0;

			if (base == 16)
			{
				auto hv = hex_value(c);
				if (!hv)
					return std::nullopt;

				digit = *hv;
			}
			else
			{
				if (c < '0' || c > '9')
					return std::nullopt;

				digit = static_cast<unsigned>(c - '0');
			}

			v = v * static_cast<unsigned>(base) + digit;
			if (v > 255)
				return std::nullopt;
		}

		return v;
	}

	[[nodiscard]] std::vector<std::uint8_t> parse_expected_text_bytes(std::string_view s, bool& ok)
	{
		ok = true;

		std::vector<std::uint8_t> out;

		auto flush_token = [&](std::string& tok) {
			if (tok.empty())
				return;

			auto v = parse_token_as_byte(tok);
			if (!v)
			{
				ok = false;
				return;
			}

			out.push_back(static_cast<std::uint8_t>(*v));
			tok.clear();
		};

		std::string tok;

		for (std::size_t i = 0; i < s.size(); ++i)
		{
			auto const c = s[i];

			if (c == ';')
			{
				flush_token(tok);
				while (i < s.size() && s[i] != '\n')
					++i;
				continue;
			}

			if (c == ',' || c == '\n' || c == '\r' || c == '\t' || c == ' ')
			{
				flush_token(tok);
				if (!ok)
					return {};

				continue;
			}

			if (is_hex_digit(c) || c == 'x' || c == 'X')
			{
				tok.push_back(c);
				continue;
			}

			flush_token(tok);
			if (!ok)
				return {};
		}

		flush_token(tok);
		if (!ok)
			return {};

		return out;
	}

	[[nodiscard]] std::vector<std::uint8_t>
	read_expected_bytes(std::filesystem::path const& expected_path, bool& ok)
	{
		ok = true;

		auto const ext = expected_path.extension().string();
		if (ext == ".bin")
		{
			auto b = read_bin_file(expected_path);
			if (b.empty())
			{
				std::ifstream f(expected_path, std::ios::binary);
				if (!f)
					ok = false;
			}
			return b;
		}

		auto text = read_text_file(expected_path);
		if (text.empty())
		{
			std::ifstream f(expected_path, std::ios::binary);
			if (!f)
				ok = false;

			return {};
		}

		return parse_expected_text_bytes(text, ok);
	}

	void emit_parse_errors(yasme::Diagnostics& diag, std::span<const yasme::ir::ParseError> errs)
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

	void print_mismatch(std::span<const std::uint8_t> got, std::span<const std::uint8_t> expected)
	{
		auto const min_len = (got.size() < expected.size()) ? got.size() : expected.size();

		for (std::size_t i = 0; i < min_len; ++i)
		{
			if (got[i] != expected[i])
			{
				std::cerr << "byte mismatch at index " << i << ": expected "
						  << static_cast<unsigned>(expected[i]) << ", got "
						  << static_cast<unsigned>(got[i]) << "\n";
				return;
			}
		}

		if (got.size() != expected.size())
		{
			std::cerr << "size mismatch: expected " << expected.size() << " bytes, got "
					  << got.size() << " bytes\n";
		}
	}

} // namespace

int main(int argc, char** argv)
{
	if (argc != 3)
	{
		std::cerr
			<< "usage: yasme_test_runner <input.asm> <expected.bytes|expected.hex|expected.bin>\n";
		return 2;
	}

	auto const asm_path = std::filesystem::path(argv[1]);
	auto const expected_path = std::filesystem::path(argv[2]);

	bool expected_ok = true;
	auto expected = read_expected_bytes(expected_path, expected_ok);
	if (!expected_ok)
	{
		std::cerr << "error: failed to read expected file: " << expected_path.string() << "\n";
		return 2;
	}

	yasme::SourceManager sources{};
	yasme::Diagnostics diag(sources, std::cerr);
	diag.set_color_mode(yasme::ColorMode::never);

	auto in_id_res = sources.open_read(asm_path.string());
	if (!in_id_res.ok())
	{
		std::cerr << "error: failed to open input: " << asm_path.string() << ": " << in_id_res.err()
				  << "\n";
		return 2;
	}

	auto const in_id = in_id_res.value();

	yasme::ir::Parser parser(sources, in_id);
	auto parse_res = parser.parse_program();

	if (!parse_res.errors.empty())
	{
		emit_parse_errors(diag, parse_res.errors);
		return 1;
	}

	yasme::Assembler assembler(diag);
	auto out = assembler.assemble(parse_res.program);

	if (diag.error_count() != 0 || out.errors != 0)
		return 1;

	auto const got = std::span<const std::uint8_t>(out.bytes.data(), out.bytes.size());
	auto const exp = std::span<const std::uint8_t>(expected.data(), expected.size());

	if (got.size() != exp.size() || !std::equal(got.begin(), got.end(), exp.begin()))
	{
		std::cerr << "FAIL: " << asm_path.filename().string() << "\n";
		print_mismatch(got, exp);
		return 1;
	}

	return 0;
}
