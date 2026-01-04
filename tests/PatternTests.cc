#include <iostream>
#include <string>
#include <string_view>
#include <yasme/macro/Pattern.hh>
#include <yasme/macro/TokenBuffer.hh>
#include <yasme/macro/TokenSlice.hh>
#include <yasme/support/SourceManager.hh>

namespace
{
	struct TokenView
	{
		yasme::macro::TokenBuffer buffer{};
		yasme::macro::TokenSlice slice{};
	};

	TokenView lex_slice(yasme::SourceManager& sources, std::string_view name, std::string_view text)
	{
		auto id = sources.add_virtual(std::string(name), std::string(text));

		TokenView view{};
		view.buffer = yasme::macro::TokenBuffer(sources, id, {});

		auto toks = view.buffer.tokens();
		auto end = toks.size();
		if (end > 0 && toks[end - 1].kind == yasme::lex::TokenKind::eof)
			--end;

		view.slice = yasme::macro::make_token_slice(toks.data(), toks.data() + end);
		return view;
	}

	bool check(bool cond, std::string_view msg)
	{
		if (cond)
			return true;

		std::cerr << "FAIL: " << msg << "\n";
		return false;
	}

	bool expect_match(std::string_view pattern_text, std::string_view input_text)
	{
		yasme::SourceManager sources{};

		auto pattern_view = lex_slice(sources, "pattern", pattern_text);
		auto input_view = lex_slice(sources, "input", input_text);

		auto parsed = yasme::macro::parse_pattern(pattern_view.slice);
		if (!parsed.ok())
			return false;

		yasme::macro::MatchResult result{};
		return yasme::macro::match_pattern(parsed.pattern, input_view.slice, result);
	}

	std::string slice_text(yasme::macro::TokenSlice slice)
	{
		return yasme::macro::token_slice_to_string(slice);
	}

} // namespace

int main()
{
	int failures = 0;

	failures += !check(expect_match("foo + 1", "foo + 1"), "literal tokens match");
	failures += !check(expect_match("_ + 1", "foo + 1"), "underscore wildcard matches");
	failures += !check(expect_match("foo ... bar", "foo + 1 + bar"), "ellipsis matches sequence");
	failures += !check(expect_match("foo ... bar", "foo bar"), "ellipsis matches empty");

	{
		struct MatchCase
		{
			std::string_view pattern{};
			std::string_view input{};
			bool expect{};
			std::string_view label{};
		};

		auto const cases = std::vector<MatchCase>{
			{"foo + 1", "foo + 1", true, "table: literal match"},
			{"foo + 1", "foo + 2", false, "table: literal mismatch"},
			{"_ + _", "a + b", true, "table: wildcard match"},
			{"... + {b}", "1 + 2", true, "table: ellipsis then bind"},
			{"{a} ... {b}", "1 2", true, "table: binds around ellipsis"},
			{"{a} ... {b}", "1", false, "table: bind needs at least one token"},
		};

		for (auto const& c : cases)
		{
			auto ok = expect_match(c.pattern, c.input);
			if (c.expect)
				failures += !check(ok, c.label);
			else
				failures += !check(!ok, c.label);
		}
	}

	{
		yasme::SourceManager sources{};
		auto pattern_view = lex_slice(sources, "pattern", "{lhs} + {rhs}");
		auto input_view = lex_slice(sources, "input", "1 + 2");

		auto parsed = yasme::macro::parse_pattern(pattern_view.slice);
		yasme::macro::MatchResult result{};
		auto ok =
			parsed.ok() && yasme::macro::match_pattern(parsed.pattern, input_view.slice, result);

		failures += !check(ok, "binding captures for simple add");
		failures += !check(slice_text(result.bindings["lhs"]) == "1", "lhs captures one token");
		failures += !check(slice_text(result.bindings["rhs"]) == "2", "rhs captures one token");
	}

	{
		yasme::SourceManager sources{};
		auto pattern_view = lex_slice(sources, "pattern", "{lhs} + {rhs}");
		auto input_view = lex_slice(sources, "input", "1 + 2 + 3");

		auto parsed = yasme::macro::parse_pattern(pattern_view.slice);
		yasme::macro::MatchResult result{};
		auto ok =
			parsed.ok() && yasme::macro::match_pattern(parsed.pattern, input_view.slice, result);

		failures += !check(ok, "binding captures longer rhs");
		failures += !check(slice_text(result.bindings["lhs"]) == "1", "lhs stays minimal");
		failures +=
			!check(slice_text(result.bindings["rhs"]) == "2 + 3", "rhs captures remaining tokens");
	}

	{
		yasme::SourceManager sources{};
		auto pattern_view = lex_slice(sources, "pattern", "{lhs + rhs");
		auto parsed = yasme::macro::parse_pattern(pattern_view.slice);
		failures += !check(!parsed.ok(), "invalid binding reports parse error");
	}

	{
		struct ParseCase
		{
			std::string_view pattern{};
			std::string_view label{};
		};

		auto const cases = std::vector<ParseCase>{
			{"{a", "table: missing closing brace"},
			{"{1}", "table: non-identifier binding name"},
			{"}", "table: stray right brace"},
			{"{...}", "table: ellipsis in binding"},
		};

		for (auto const& c : cases)
		{
			yasme::SourceManager sources{};
			auto view = lex_slice(sources, "pattern", c.pattern);
			auto parsed = yasme::macro::parse_pattern(view.slice);
			failures += !check(!parsed.ok(), c.label);
		}
	}

	failures += !check(!expect_match("_", ""), "wildcard does not match empty");
	failures += !check(expect_match("...", ""), "ellipsis matches empty input");

	if (failures != 0)
		std::cerr << failures << " test(s) failed\n";

	return failures == 0 ? 0 : 1;
}
