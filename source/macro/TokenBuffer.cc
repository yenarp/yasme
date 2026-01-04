#include <yasme/macro/TokenBuffer.hh>

namespace yasme::macro
{
	TokenBuffer::TokenBuffer(SourceManager const& sources, FileId file, lex::LexerOptions opt)
	{
		reset(sources, file, opt);
	}

	void TokenBuffer::reset(SourceManager const& sources, FileId file, lex::LexerOptions opt)
	{
		m_tokens.clear();
		m_errors.clear();

		lex::Lexer lex(sources, file, opt);

		for (;;)
		{
			auto tok = lex.next();
			m_tokens.push_back(tok);
			if (tok.is(lex::TokenKind::eof))
				break;
		}

		auto errs = lex.errors();
		m_errors.assign(errs.begin(), errs.end());
	}

} // namespace yasme::macro
