#ifndef YASME_SUPPORT_CTYPE_HH
#define YASME_SUPPORT_CTYPE_HH

static constexpr bool is_ascii_alpha(char c) noexcept
{
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static constexpr bool is_ascii_digit(char c) noexcept
{
	return c >= '0' && c <= '9';
}

static constexpr bool is_ascii_alnum(char c) noexcept
{
	return is_ascii_alpha(c) || is_ascii_digit(c);
}

static constexpr char ascii_tolower(char c) noexcept
{
	if (c >= 'A' && c <= 'Z')
		return static_cast<char>(c - 'A' + 'a');

	return c;
}

static constexpr bool is_ident_start(char c) noexcept
{
	return is_ascii_alpha(c) || c == '_' || c == '.';
}

static constexpr bool is_ident_continue(char c) noexcept
{
	return is_ascii_alnum(c) || c == '_' || c == '.';
}

#endif /* YASME_SUPPORT_CTYPE_HH */
