#include <algorithm>
#include <cctype>
#include <cstdint>
#include <limits>
#include <optional>
#include <span>
#include <string_view>
#include <utility>
#include <yasme/asm/Assembler.hh>

namespace yasme
{
	namespace
	{
		template <class... Ts> struct Overload : Ts...
		{
			using Ts::operator()...;
		};
		template <class... Ts> Overload(Ts...) -> Overload<Ts...>;

		thread_local bool g_emit_diagnostics = false;
		thread_local bool g_error_unresolved = false;

		[[nodiscard]] std::uint64_t fnv1a_bytes(std::span<const std::uint8_t> bytes) noexcept
		{
			std::uint64_t h = 14695981039346656037ull;
			for (auto b : bytes)
			{
				h ^= static_cast<std::uint64_t>(b);
				h *= 1099511628211;
			}
			return h;
		}

		[[nodiscard]] std::uint64_t fnv1a_str(std::string_view s) noexcept
		{
			std::uint64_t h = 14695981039346656037ull;
			for (auto ch : s)
			{
				h ^= static_cast<std::uint64_t>(static_cast<unsigned char>(ch));
				h *= 1099511628211;
			}
			return h;
		}

		[[nodiscard]] std::uint64_t hash_u64(std::uint64_t x) noexcept
		{
			x += 0x9e3779b97f4a7c15;
			x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9;
			x = (x ^ (x >> 27)) * 0x94d049bb133111eb;
			return x ^ (x >> 31);
		}

		[[nodiscard]] bool fits_i64(std::uint64_t v) noexcept
		{
			return v <= static_cast<std::uint64_t>(std::numeric_limits<std::int64_t>::max());
		}

		[[nodiscard]] std::optional<std::int64_t> to_i64(std::uint64_t v) noexcept
		{
			if (!fits_i64(v))
				return std::nullopt;

			return static_cast<std::int64_t>(v);
		}

		[[nodiscard]] std::string value_to_string(Assembler::Value const& v)
		{
			if (Assembler::is_int(v))
				return std::to_string(std::get<std::int64_t>(v));

			if (Assembler::is_str(v))
				return std::get<std::string>(v);

			return {};
		}

	} // namespace

	Assembler::Assembler(Diagnostics& diag) noexcept : m_diag(std::addressof(diag))
	{
	}

	AssembleOutput Assembler::assemble(ir::Program const& program, AssembleOptions opt)
	{
		auto const err_before = m_diag->error_count();
		std::size_t detection_passes_ran = 0;

		PassState last{};
		std::size_t last_fp{};
		bool have_last = false;
		bool stable = false;

		auto const detection_limit = (opt.max_passes > 0) ? (opt.max_passes - 1) : 0;

		for (std::size_t pass = 0; pass < detection_limit; ++pass)
		{
			++detection_passes_ran;
			auto const seed = have_last ? seed_next_pass(last) : seed_next_pass(PassState{});

			g_emit_diagnostics = false;
			g_error_unresolved = false;

			auto cur = run_pass(program, seed);
			auto const fp = fingerprint(cur);

			if (have_last && fp == last_fp)
			{
				stable = true;
				last = std::move(cur);
				last_fp = fp;
				break;
			}

			have_last = true;
			last = std::move(cur);
			last_fp = fp;
		}

		auto const final_seed = have_last ? seed_next_pass(last) : seed_next_pass(PassState{});

		g_emit_diagnostics = true;
		g_error_unresolved = opt.error_on_unresolved;

		auto final_state = run_pass(program, final_seed);
		auto const fp_final = fingerprint(final_state);

		if (!stable && have_last && fp_final == last_fp)
			stable = true;

		if (stable && opt.run_final_postpone)
			run_postpone_after_stable(final_state);

		if (!stable && opt.max_passes > 0)
		{
			SourceSpan anchor{};
			if (!program.stmts.empty() && program.stmts.front())
			{
				std::visit(Overload{[&](auto const& s) { anchor = s.span; }},
						   program.stmts.front()->node);
			}
			error(
				final_state, anchor, "assembly did not stabilize within the configured pass limit");
		}

		AssembleOutput out{};
		out.passes = detection_passes_ran + 1;

		auto it = final_state.streams.find("output");
		if (it != final_state.streams.end())
			out.bytes = std::move(it->second.bytes);

		out.errors = m_diag->error_count() - err_before;
		return out;
	}

	Assembler::PassState Assembler::run_pass(ir::Program const& program, PassState const& seed)
	{
		PassState st = seed;
		st.postpone_each_pass.clear();
		st.postpone_after_stable.clear();

		if (!st.streams.contains("output"))
			st.streams.emplace("output", Stream{.name = "output"});

		st.current_stream = "output";
		st.dollar_address = cur_stream(st).origin + cur_stream(st).bytes.size();

		auto walk_prog = [&](ir::Program const& p) {
			for (auto const& sp : p.stmts)
			{
				if (!sp)
					continue;
				walk_stmt(st, *sp);
			}
		};

		walk_prog(program);

		run_postpone_each_pass(st);
		return st;
	}

	Assembler::Flow Assembler::walk_stmt_cf(PassState& st, ir::Stmt const& stmt)
	{
		return std::visit(
			Overload{
				[&](ir::StmtOrg const& s) -> Flow {
					apply_org(st, s);
					return {};
				},
				[&](ir::StmtLabel const& s) -> Flow {
					apply_label(st, s);
					return {};
				},
				[&](ir::StmtAssign const& s) -> Flow {
					apply_assign(st, s);
					return {};
				},
				[&](ir::StmtDefine const& s) -> Flow {
					apply_define(st, s);
					return {};
				},
				[&](ir::StmtEmitData const& s) -> Flow {
					apply_emit_data(st, s);
					return {};
				},
				[&](ir::StmtLoad const& s) -> Flow {
					apply_load(st, s);
					return {};
				},
				[&](ir::StmtVirtual const& s) -> Flow {
					apply_virtual(st, s);
					return {};
				},
				[&](ir::StmtPostpone const& s) -> Flow {
					apply_postpone(st, s);
					return {};
				},
				[&](ir::StmtIf const& s) -> Flow { return apply_if(st, s); },
				[&](ir::StmtRepeat const& s) -> Flow { return apply_repeat(st, s); },
				[&](ir::StmtWhile const& s) -> Flow { return apply_while(st, s); },
				[&](ir::StmtForNumeric const& s) -> Flow { return apply_for_numeric(st, s); },
				[&](ir::StmtForChars const& s) -> Flow { return apply_for_chars(st, s); },
				[&](ir::StmtError const& s) -> Flow {
					apply_error(st, s);
					return {};
				},
				[&](ir::StmtBreak const& s) -> Flow {
					Flow f{};
					f.kind = Flow::Kind::break_;
					f.span = s.span;
					return f;
				},
				[&](ir::StmtContinue const& s) -> Flow {
					Flow f{};
					f.kind = Flow::Kind::continue_;
					f.span = s.span;
					return f;
				},
				[&](ir::StmtEnd const&) -> Flow { return {}; },
			},
			stmt.node);
	}

	Assembler::Flow Assembler::walk_block_cf(PassState& st, std::vector<ir::StmtPtr> const& body)
	{
		for (auto const& sp : body)
		{
			if (!sp)
				continue;
			auto f = walk_stmt_cf(st, *sp);
			if (f.kind != Flow::Kind::none)
				return f;
		}
		return {};
	}

	void Assembler::walk_stmt(PassState& st, ir::Stmt const& stmt)
	{
		auto f = walk_stmt_cf(st, stmt);
		if (f.kind != Flow::Kind::none)
			error(st, f.span, "break/continue used outside of a loop");
	}

	void Assembler::walk_block(PassState& st, std::vector<ir::StmtPtr> const& body)
	{
		auto f = walk_block_cf(st, body);
		if (f.kind != Flow::Kind::none)
			error(st, f.span, "break/continue used outside of a loop");
	}

	void Assembler::run_postpone_each_pass(PassState& st)
	{
		for (std::size_t i = 0; i < st.postpone_each_pass.size(); ++i)
		{
			auto const* p = st.postpone_each_pass[i];
			if (!p)
				continue;

			auto f = walk_block_cf(st, p->body);
			if (f.kind != Flow::Kind::none)
				error(st, f.span, "break/continue used outside of a loop");
		}
	}

	void Assembler::run_postpone_after_stable(PassState& st)
	{
		for (std::size_t i = 0; i < st.postpone_after_stable.size(); ++i)
		{
			auto const* p = st.postpone_after_stable[i];
			if (!p)
				continue;

			auto f = walk_block_cf(st, p->body);
			if (f.kind != Flow::Kind::none)
				error(st, f.span, "break/continue used outside of a loop");
		}
	}

	void Assembler::apply_org(PassState& st, ir::StmtOrg const& s)
	{
		auto v = eval_value(st, s.address);
		if (is_unknown(v) || !is_int(v))
		{
			if (g_error_unresolved)
				error(st, s.span, "org expects a resolved numeric expression");

			return;
		}

		auto const addr_i = std::get<std::int64_t>(v);
		if (addr_i < 0)
		{
			error(st, s.span, "org address must be non-negative");
			return;
		}

		auto& stream = cur_stream(st);
		auto const addr = static_cast<std::uint64_t>(addr_i);
		auto const off = static_cast<std::uint64_t>(stream.bytes.size());
		if (addr < off)
		{
			error(st, s.span, "org address is behind the current position");
			return;
		}

		stream.origin = addr - off;
		st.dollar_address = addr;
	}

	void Assembler::apply_label(PassState& st, ir::StmtLabel const& s)
	{
		if (!ir::is_valid_identifier(s.name))
		{
			error(st, s.span, "invalid label identifier");
			return;
		}

		auto& sym = st.symbols[s.name];

		if (sym.kind == SymbolKind::define_numeric || sym.kind == SymbolKind::define_string)
		{
			error(st, s.span, "cannot redefine a define as a label");
			return;
		}

		sym.kind = SymbolKind::label;

		auto const v = to_i64(st.dollar_address);
		if (!v)
		{
			error(st, s.span, "label address does not fit in signed 64-bit");
			sym.value = Unknown{};
		}
		else
		{
			sym.value = *v;
		}

		sym.pointee.clear();
		sym.declared_at = s.span;
	}

	void Assembler::apply_assign(PassState& st, ir::StmtAssign const& s)
	{
		if (!ir::is_valid_identifier(s.name))
		{
			error(st, s.span, "invalid identifier in assignment");
			return;
		}

		auto it = st.symbols.find(s.name);
		if (it != st.symbols.end()
			&& (it->second.kind == SymbolKind::define_numeric
				|| it->second.kind == SymbolKind::define_string))
		{
			error(st, s.span, "cannot assign to a define");
			return;
		}

		Symbol sym{};
		sym.declared_at = s.span;

		if (ir::is_name_expr(s.rhs))
		{
			sym.kind = SymbolKind::symbolic;
			auto name = eval_name(st, s.rhs);
			if (!name)
				sym.pointee.clear();
			else
			{
				if (!ir::is_valid_identifier(*name))
					error(st, s.rhs.span, "name expression produced an invalid identifier");

				sym.pointee = std::move(*name);
			}
			sym.value = Unknown{};
		}
		else
		{
			auto v = eval_value(st, s.rhs);
			if (is_str(v))
				sym.kind = SymbolKind::string;
			else
				sym.kind = SymbolKind::numeric;

			sym.value = std::move(v);
			sym.pointee.clear();
		}

		st.symbols[s.name] = std::move(sym);
	}

	void Assembler::apply_define(PassState& st, ir::StmtDefine const& s)
	{
		if (!ir::is_valid_identifier(s.name))
		{
			error(st, s.span, "invalid identifier in define");
			return;
		}

		auto it = st.symbols.find(s.name);
		if (it != st.symbols.end())
		{
			auto const k = it->second.kind;
			if (k != SymbolKind::define_numeric && k != SymbolKind::define_string)
			{
				error(st, s.span, "redefinition of a symbol");
				return;
			}

			if (ir::is_name_expr(s.value))
			{
				error(st,
					  s.value.span,
					  "define value must be a numeric or string constant, not a name expression");
				return;
			}

			auto v = eval_value(st, s.value);
			if (is_unknown(v))
			{
				error(st, s.value.span, "define value must be fully resolved in the current pass");
				return;
			}

			if (k == SymbolKind::define_numeric && is_int(v) && is_int(it->second.value)
				&& std::get<std::int64_t>(v) == std::get<std::int64_t>(it->second.value))
				return;

			if (k == SymbolKind::define_string && is_str(v) && is_str(it->second.value)
				&& std::get<std::string>(v) == std::get<std::string>(it->second.value))
				return;

			error(st, s.span, "redefinition of a symbol");
			return;
		}

		if (ir::is_name_expr(s.value))
		{
			error(st,
				  s.value.span,
				  "define value must be a numeric or string constant, not a name expression");

			return;
		}

		auto v = eval_value(st, s.value);
		if (is_unknown(v))
		{
			error(st, s.value.span, "define value must be fully resolved in the current pass");
			return;
		}

		Symbol sym{};
		sym.declared_at = s.span;
		sym.pointee.clear();

		if (is_int(v))
		{
			sym.kind = SymbolKind::define_numeric;
			sym.value = std::get<std::int64_t>(v);
		}
		else if (is_str(v))
		{
			sym.kind = SymbolKind::define_string;
			sym.value = std::get<std::string>(v);
		}
		else
		{
			error(st, s.value.span, "define produced an unsupported constant type");
			return;
		}

		st.symbols.emplace(s.name, std::move(sym));
	}

	void Assembler::apply_emit_data(PassState& st, ir::StmtEmitData const& s)
	{
		for (auto const& item : s.items)
		{
			auto v = eval_value(st, item);

			if (is_unknown(v))
			{
				if (g_error_unresolved)
					error(st, item.span, "unresolved expression in data emission");

				switch (s.unit)
				{
					case ir::DataUnit::u8:
						emit_u8(st, 0);
						break;
					case ir::DataUnit::u16:
						emit_u16(st, 0);
						break;
					case ir::DataUnit::u32:
						emit_u32(st, 0);
						break;
					case ir::DataUnit::u64:
						emit_u64(st, 0);
						break;
				}
				continue;
			}

			if (is_str(v))
			{
				auto const& str = std::get<std::string>(v);
				if (s.unit != ir::DataUnit::u8)
				{
					error(st, item.span, "string values are only supported in db");
					continue;
				}

				for (unsigned char ch : str)
					emit_u8(st, static_cast<std::uint8_t>(ch));

				continue;
			}

			if (!is_int(v))
			{
				error(st, item.span, "data directive expects numeric or (for db) string values");
				continue;
			}

			auto const iv = std::get<std::int64_t>(v);
			auto const u = static_cast<std::uint64_t>(iv);

			switch (s.unit)
			{
				case ir::DataUnit::u8:
					emit_u8(st, static_cast<std::uint8_t>(u & 255));
					break;
				case ir::DataUnit::u16:
					emit_u16(st, static_cast<std::uint16_t>(u & 65535));
					break;
				case ir::DataUnit::u32:
					emit_u32(st, static_cast<std::uint32_t>(u & 4294967295));
					break;
				case ir::DataUnit::u64:
					emit_u64(st, static_cast<std::uint64_t>(u));
					break;
			}
		}
	}

	void Assembler::apply_load(PassState& st, ir::StmtLoad const& s)
	{
		if (!ir::is_valid_identifier(s.dest))
		{
			error(st, s.span, "invalid identifier in load destination");
			return;
		}

		auto it = st.symbols.find(s.dest);
		if (it != st.symbols.end()
			&& (it->second.kind == SymbolKind::define_numeric
				|| it->second.kind == SymbolKind::define_string))
		{
			error(st, s.span, "cannot assign to a define");
			return;
		}

		auto set_unknown = [&]() {
			Symbol sym{};
			sym.kind = SymbolKind::numeric;
			sym.value = Unknown{};
			sym.declared_at = s.span;
			st.symbols[s.dest] = std::move(sym);
		};

		std::optional<std::string> stream_name{};
		if (auto const* lit = std::get_if<ir::ExprStr>(&s.stream.node))
			stream_name = lit->value;
		else
			stream_name = eval_name(st, s.stream);

		if (!stream_name)
		{
			if (g_error_unresolved)
				error(st, s.stream.span, "load stream must resolve to an identifier or string");
			set_unknown();
			return;
		}

		auto stream_it = st.streams.find(*stream_name);
		if (stream_it == st.streams.end())
		{
			error(st, s.stream.span, "unknown stream '" + *stream_name + "'");
			set_unknown();
			return;
		}

		auto offset_v = eval_value(st, s.offset);
		if (is_unknown(offset_v))
		{
			if (g_error_unresolved)
				error(st, s.offset.span, "load offset expects a resolved numeric expression");
			set_unknown();
			return;
		}

		if (!is_int(offset_v))
		{
			error(st, s.offset.span, "load offset expects a numeric expression");
			set_unknown();
			return;
		}

		auto const offset_i = std::get<std::int64_t>(offset_v);
		if (offset_i < 0)
		{
			error(st, s.offset.span, "load offset must be non-negative");
			set_unknown();
			return;
		}

		std::size_t width = 1;
		switch (s.unit)
		{
			case ir::DataUnit::u8:
				width = 1;
				break;
			case ir::DataUnit::u16:
				width = 2;
				break;
			case ir::DataUnit::u32:
				width = 4;
				break;
			case ir::DataUnit::u64:
				width = 8;
				break;
		}

		auto const off = static_cast<std::uint64_t>(offset_i);
		auto const end = off + static_cast<std::uint64_t>(width);
		if (end > stream_it->second.bytes.size())
		{
			error(st, s.span, "load offset is out of bounds for stream");
			return;
		}

		std::uint64_t value = 0;
		for (std::size_t i = 0; i < width; ++i)
		{
			auto byte = stream_it->second.bytes[static_cast<std::size_t>(off + i)];
			value |= static_cast<std::uint64_t>(byte) << (8 * i);
		}

		auto const as_i64 = to_i64(value);
		if (!as_i64)
		{
			error(st, s.span, "load value does not fit in signed 64-bit");
			set_unknown();
			return;
		}

		Symbol sym{};
		sym.kind = SymbolKind::numeric;
		sym.value = *as_i64;
		sym.declared_at = s.span;
		st.symbols[s.dest] = std::move(sym);
	}

	void Assembler::apply_virtual(PassState& st, ir::StmtVirtual const& s)
	{
		auto old_stream = st.current_stream;

		std::string name;
		if (s.name_expr)
		{
			auto nm = eval_name(st, *s.name_expr);
			if (nm && ir::is_valid_identifier(*nm))
				name = std::move(*nm);
			else
				name = "virtual_" + std::to_string(s.span.begin.offset);
		}
		else
		{
			name = "virtual_" + std::to_string(s.span.begin.offset);
		}

		auto [it, inserted] =
			st.streams.try_emplace(name, Stream{.name = name, .bytes = {}, .origin = 0});
		(void)inserted;

		st.current_stream = name;
		st.dollar_address = it->second.origin + it->second.bytes.size();

		auto f = walk_block_cf(st, s.body);
		if (f.kind != Flow::Kind::none)
			error(st, f.span, "break/continue used outside of a loop");

		st.current_stream = old_stream;
		st.dollar_address = cur_stream(st).origin + cur_stream(st).bytes.size();
	}

	void Assembler::apply_postpone(PassState& st, ir::StmtPostpone const& s)
	{
		if (s.mode == ir::PostponeMode::at_end_each_pass)
			st.postpone_each_pass.push_back(std::addressof(s));
		else
			st.postpone_after_stable.push_back(std::addressof(s));
	}

	static std::optional<std::int64_t> add_i64_checked(std::int64_t a, std::int64_t b) noexcept
	{
		__int128 v = static_cast<__int128>(a) + static_cast<__int128>(b);
		if (v < static_cast<__int128>(std::numeric_limits<std::int64_t>::min()))
			return std::nullopt;
		if (v > static_cast<__int128>(std::numeric_limits<std::int64_t>::max()))
			return std::nullopt;
		return static_cast<std::int64_t>(v);
	}

	static std::optional<std::int64_t> sub_i64_checked(std::int64_t a, std::int64_t b) noexcept
	{
		__int128 v = static_cast<__int128>(a) - static_cast<__int128>(b);
		if (v < static_cast<__int128>(std::numeric_limits<std::int64_t>::min()))
			return std::nullopt;
		if (v > static_cast<__int128>(std::numeric_limits<std::int64_t>::max()))
			return std::nullopt;
		return static_cast<std::int64_t>(v);
	}

	static std::optional<std::int64_t> mul_i64_checked(std::int64_t a, std::int64_t b) noexcept
	{
		__int128 v = static_cast<__int128>(a) * static_cast<__int128>(b);
		if (v < static_cast<__int128>(std::numeric_limits<std::int64_t>::min()))
			return std::nullopt;
		if (v > static_cast<__int128>(std::numeric_limits<std::int64_t>::max()))
			return std::nullopt;
		return static_cast<std::int64_t>(v);
	}

	Assembler::Flow Assembler::apply_if(PassState& st, ir::StmtIf const& s)
	{
		auto v = eval_value(st, s.cond);
		if (is_unknown(v))
		{
			if (g_error_unresolved)
				error(st, s.cond.span, "if condition expects a resolved numeric expression");
			return {};
		}

		if (!is_int(v))
		{
			error(st, s.cond.span, "if condition expects a numeric expression");
			return {};
		}

		auto const cond = std::get<std::int64_t>(v);

		if (cond != 0)
			return walk_block_cf(st, s.then_body);

		if (s.has_else)
			return walk_block_cf(st, s.else_body);

		return {};
	}

	Assembler::Flow Assembler::apply_repeat(PassState& st, ir::StmtRepeat const& s)
	{
		auto v = eval_value(st, s.count);
		if (is_unknown(v))
		{
			if (g_error_unresolved)
				error(st, s.count.span, "repeat count expects a resolved numeric expression");
			return {};
		}

		if (!is_int(v))
		{
			error(st, s.count.span, "repeat count expects a numeric expression");
			return {};
		}

		auto const count = std::get<std::int64_t>(v);
		if (count < 0)
		{
			error(st, s.count.span, "repeat count must be non-negative");
			return {};
		}

		std::size_t limit = 1000000;
		if (static_cast<std::uint64_t>(count) > limit)
		{
			error(st, s.count.span, "repeat iteration count is too large");
			return {};
		}

		for (std::int64_t i = 0; i < count; ++i)
		{
			auto f = walk_block_cf(st, s.body);
			if (f.kind == Flow::Kind::break_)
				return {};
			if (f.kind == Flow::Kind::continue_)
				continue;
			if (f.kind != Flow::Kind::none)
				return f;
		}
		return {};
	}

	Assembler::Flow Assembler::apply_for_numeric(PassState& st, ir::StmtForNumeric const& s)
	{
		auto v_start = eval_value(st, s.start);
		auto v_end = eval_value(st, s.end);

		if (is_unknown(v_start) || is_unknown(v_end))
		{
			if (g_error_unresolved)
				error(st, s.span, "for bounds must be resolved in this pass");
			return {};
		}

		if (!is_int(v_start) || !is_int(v_end))
		{
			error(st, s.span, "for bounds must be numeric");
			return {};
		}

		auto const start = std::get<std::int64_t>(v_start);
		auto const end = std::get<std::int64_t>(v_end);

		std::int64_t step = 0;
		if (s.step)
		{
			auto v_step = eval_value(st, *s.step);
			if (is_unknown(v_step))
			{
				if (g_error_unresolved)
					error(st, s.span, "for step must be resolved in this pass");
				return {};
			}
			if (!is_int(v_step))
			{
				error(st, s.span, "for step must be numeric");
				return {};
			}
			step = std::get<std::int64_t>(v_step);
		}
		else
			step = (start <= end) ? 1 : -1;

		if (step == 0)
		{
			error(st, s.span, "for step must be non-zero");
			return {};
		}

		struct Restore
		{
			bool had{};
			Symbol sym{};
		};

		Restore restore{};
		auto it_old = st.symbols.find(s.var);
		if (it_old != st.symbols.end())
		{
			restore.had = true;
			restore.sym = it_old->second;
		}

		auto set_var = [&](std::int64_t v) {
			Symbol sym{};
			sym.kind = SymbolKind::numeric;
			sym.value = v;
			sym.declared_at = s.span;
			st.symbols[s.var] = std::move(sym);
		};

		std::size_t iters = 0;
		std::size_t limit = 1000000;

		auto should_continue = [&](std::int64_t cur) -> bool {
			if (step > 0)
				return cur <= end;
			return cur >= end;
		};

		for (std::int64_t cur = start; should_continue(cur);)
		{
			if (iters++ >= limit)
			{
				error(st, s.span, "for loop exceeded iteration limit");
				break;
			}

			set_var(cur);

			auto f = walk_block_cf(st, s.body);
			if (f.kind == Flow::Kind::break_)
				break;
			if (f.kind == Flow::Kind::continue_)
			{
				auto next = add_i64_checked(cur, step);
				if (!next)
				{
					error(st, s.span, "for loop induction variable overflow");
					break;
				}
				cur = *next;
				continue;
			}
			if (f.kind != Flow::Kind::none)
			{
				if (restore.had)
					st.symbols[s.var] = std::move(restore.sym);
				else
					st.symbols.erase(s.var);
				return f;
			}

			auto next = add_i64_checked(cur, step);
			if (!next)
			{
				error(st, s.span, "for loop induction variable overflow");
				break;
			}
			cur = *next;
		}

		if (restore.had)
			st.symbols[s.var] = std::move(restore.sym);
		else
			st.symbols.erase(s.var);

		return {};
	}

	Assembler::Flow Assembler::apply_for_chars(PassState& st, ir::StmtForChars const& s)
	{
		auto v = eval_value(st, s.str);
		if (is_unknown(v))
		{
			if (g_error_unresolved)
				error(st, s.str.span, "for-in expects a resolved string expression");
			return {};
		}

		if (!is_str(v))
		{
			error(st, s.str.span, "for-in expects a string expression");
			return {};
		}

		auto const& str = std::get<std::string>(v);

		struct Restore
		{
			bool had{};
			Symbol sym{};
		};

		Restore restore{};
		auto it_old = st.symbols.find(s.var);
		if (it_old != st.symbols.end())
		{
			restore.had = true;
			restore.sym = it_old->second;
		}

		auto set_var = [&](char c) {
			Symbol sym{};
			sym.kind = SymbolKind::string;
			sym.value = std::string(1, c);
			sym.declared_at = s.span;
			st.symbols[s.var] = std::move(sym);
		};

		std::size_t iters = 0;
		std::size_t limit = 1000000;

		for (char c : str)
		{
			if (iters++ >= limit)
			{
				error(st, s.span, "for loop exceeded iteration limit");
				break;
			}

			set_var(c);

			auto f = walk_block_cf(st, s.body);
			if (f.kind == Flow::Kind::break_)
				break;
			if (f.kind == Flow::Kind::continue_)
				continue;
			if (f.kind != Flow::Kind::none)
			{
				if (restore.had)
					st.symbols[s.var] = std::move(restore.sym);
				else
					st.symbols.erase(s.var);
				return f;
			}
		}

		if (restore.had)
			st.symbols[s.var] = std::move(restore.sym);
		else
			st.symbols.erase(s.var);

		return {};
	}

	Assembler::Flow Assembler::apply_while(PassState& st, ir::StmtWhile const& s)
	{
		auto init = eval_value(st, s.cond);
		if (is_unknown(init))
		{
			if (g_error_unresolved)
				error(st, s.cond.span, "while condition expects a resolved numeric expression");
			return {};
		}

		if (!is_int(init))
		{
			error(st, s.cond.span, "while condition expects a numeric expression");
			return {};
		}

		PassState tmp = st;

		std::size_t iters = 0;
		std::size_t limit = 1000000;

		for (;;)
		{
			auto v = eval_value(tmp, s.cond);
			if (is_unknown(v))
			{
				if (g_error_unresolved)
					error(st, s.cond.span, "while condition is unresolved in this pass");
				return {};
			}
			if (!is_int(v))
			{
				error(st, s.cond.span, "while condition expects a numeric expression");
				return {};
			}

			if (std::get<std::int64_t>(v) == 0)
				break;

			if (iters++ >= limit)
			{
				error(st, s.span, "while loop exceeded iteration limit");
				return {};
			}

			auto f = walk_block_cf(tmp, s.body);
			if (f.kind == Flow::Kind::break_)
				break;
			if (f.kind == Flow::Kind::continue_)
				continue;
			if (f.kind != Flow::Kind::none)
				return f;
		}

		st = std::move(tmp);
		return {};
	}

	namespace
	{
		[[nodiscard]] LabelKind to_label_kind(ir::DiagKind k) noexcept
		{
			switch (k)
			{
				case ir::DiagKind::note:
					return LabelKind::note;
				case ir::DiagKind::help:
					return LabelKind::help;
				case ir::DiagKind::suggestion:
					return LabelKind::suggestion;
				case ir::DiagKind::reference:
					return LabelKind::reference;
			}
			return LabelKind::note;
		}

		[[nodiscard]] AdviceKind to_advice_kind(ir::DiagKind k) noexcept
		{
			switch (k)
			{
				case ir::DiagKind::note:
					return AdviceKind::note;
				case ir::DiagKind::help:
					return AdviceKind::help;
				case ir::DiagKind::suggestion:
					return AdviceKind::suggestion;
				case ir::DiagKind::reference:
					return AdviceKind::note;
			}
			return AdviceKind::note;
		}

		[[nodiscard]] std::string diag_value_to_string(Assembler::Value const& v)
		{
			if (Assembler::is_str(v))
				return std::get<std::string>(v);
			if (Assembler::is_int(v))
				return std::to_string(std::get<std::int64_t>(v));
			return {};
		}
	} // namespace

	void Assembler::apply_error(PassState& st, ir::StmtError const& s)
	{
		Diagnostic d{};
		d.level = DiagnosticLevel::error;

		auto msg_v = eval_value(st, s.message);
		if (is_unknown(msg_v))
			d.message = "error (message unresolved)";
		else
		{
			auto str = diag_value_to_string(msg_v);
			d.message = str.empty() ? std::string("error") : std::move(str);
		}

		if (s.primary.id != 0)
			d.primary = s.primary;
		else
			d.primary = s.span;

		for (auto const& item : s.items)
		{
			auto item_v = eval_value(st, item.message);
			std::string text{};
			if (is_unknown(item_v))
				text = "unresolved diagnostic message";
			else
			{
				text = diag_value_to_string(item_v);
				if (text.empty())
					text = "invalid diagnostic message";
			}

			if (item.label_span && item.label_span->id != 0)
			{
				d.labels.push_back(DiagnosticLabel{
					std::move(text),
					*item.label_span,
					to_label_kind(item.kind),
				});
			}
			else
			{
				d.advices.push_back(DiagnosticAdvice{
					to_advice_kind(item.kind),
					std::move(text),
				});
			}
		}

		emit_diag(st, std::move(d));
	}

	Assembler::Value Assembler::eval_builtin(PassState& st, ir::ExprBuiltin const& b)
	{
		switch (b.kind)
		{
			case ir::BuiltinKind::dollar_address: {
				auto const v = to_i64(st.dollar_address);
				if (!v)
					return Unknown{};

				return *v;
			}
			case ir::BuiltinKind::stream_offset: {
				auto const off = static_cast<std::uint64_t>(cur_stream(st).bytes.size());
				auto const v = to_i64(off);
				if (!v)
					return Unknown{};

				return *v;
			}
		}
		return Unknown{};
	}

	namespace
	{
		struct SymEvalCtx
		{
			std::vector<std::string_view> stack{};
			std::size_t depth{};
		};

		[[nodiscard]] bool in_stack(SymEvalCtx const& ctx, std::string_view name)
		{
			return std::find(ctx.stack.begin(), ctx.stack.end(), name) != ctx.stack.end();
		}

	} // namespace

	[[nodiscard]] static Assembler::Value eval_ident_value(Assembler& self,
														   Assembler::PassState& st,
														   std::string_view name,
														   SourceSpan span,
														   SymEvalCtx& ctx)
	{
		auto it = st.symbols.find(std::string(name));
		if (it == st.symbols.end())
			return Assembler::Unknown{};

		auto const& sym = it->second;

		if (sym.kind == Assembler::SymbolKind::symbolic)
		{
			if (sym.pointee.empty())
				return Assembler::Unknown{};

			if (ctx.depth > 64 || in_stack(ctx, sym.pointee))
			{
				self.error(st, span, "symbolic dereference cycle detected");
				return Assembler::Unknown{};
			}

			ctx.stack.push_back(sym.pointee);
			++ctx.depth;
			auto v = eval_ident_value(self, st, sym.pointee, span, ctx);
			--ctx.depth;
			ctx.stack.pop_back();
			return v;
		}

		if (Assembler::is_unknown(sym.value))
			return Assembler::Unknown{};

		return sym.value;
	}

	Assembler::Value Assembler::eval_value(PassState& st, ir::Expr const& e)
	{
		return std::visit(
			Overload{
				[&](ir::ExprIdent const& id) -> Value {
					SymEvalCtx ctx{};
					ctx.stack.push_back(id.name);
					return eval_ident_value(*this, st, id.name, e.span, ctx);
				},
				[&](ir::ExprInt const& i) -> Value { return i.value; },
				[&](ir::ExprStr const& s) -> Value { return s.value; },
				[&](ir::ExprBuiltin const& b) -> Value { return eval_builtin(st, b); },
				[&](ir::ExprUnary const& u) -> Value {
					auto rhs = eval_value(st, *u.rhs);

					switch (u.op)
					{
						case ir::UnaryOp::plus: {
							if (is_unknown(rhs))
								return Unknown{};

							if (!is_int(rhs))
							{
								error(st, e.span, "unary + expects an integer");
								return Unknown{};
							}
							return std::get<std::int64_t>(rhs);
						}
						case ir::UnaryOp::minus: {
							if (is_unknown(rhs))
								return Unknown{};

							if (!is_int(rhs))
							{
								error(st, e.span, "unary - expects an integer");
								return Unknown{};
							}
							auto const v = std::get<std::int64_t>(rhs);
							if (v == std::numeric_limits<std::int64_t>::min())
							{
								error(st, e.span, "integer overflow in unary -");
								return Unknown{};
							}
							return -v;
						}
						case ir::UnaryOp::bit_not: {
							if (is_unknown(rhs))
								return Unknown{};

							if (!is_int(rhs))
							{
								error(st, e.span, "unary ~ expects an integer");
								return Unknown{};
							}
							return ~std::get<std::int64_t>(rhs);
						}
						case ir::UnaryOp::log_not: {
							if (is_unknown(rhs))
								return Unknown{};

							if (!is_int(rhs))
							{
								error(st, e.span, "unary ! expects an integer");
								return Unknown{};
							}
							return (std::get<std::int64_t>(rhs) == 0) ? 1 : 0;
						}
						case ir::UnaryOp::at: {
							auto nm = eval_name(st, e);
							if (!nm)
								return Unknown{};

							return *nm;
						}
					}

					return Unknown{};
				},
				[&](ir::ExprBinary const& b) -> Value {
					auto const op = b.op;

					if (op == ir::BinaryOp::log_and)
					{
						auto lhs = eval_value(st, *b.lhs);
						if (is_unknown(lhs))
							return Unknown{};

						if (!is_int(lhs))
						{
							error(st, e.span, "operator && expects integers");
							return Unknown{};
						}
						if (std::get<std::int64_t>(lhs) == 0)
							return std::int64_t{0};

						auto rhs = eval_value(st, *b.rhs);
						if (is_unknown(rhs))
							return Unknown{};

						if (!is_int(rhs))
						{
							error(st, e.span, "operator && expects integers");
							return Unknown{};
						}
						return (std::get<std::int64_t>(rhs) != 0) ? 1 : 0;
					}

					if (op == ir::BinaryOp::log_or)
					{
						auto lhs = eval_value(st, *b.lhs);
						if (is_unknown(lhs))
							return Unknown{};

						if (!is_int(lhs))
						{
							error(st, e.span, "operator || expects integers");
							return Unknown{};
						}
						if (std::get<std::int64_t>(lhs) != 0)
							return std::int64_t{1};

						auto rhs = eval_value(st, *b.rhs);
						if (is_unknown(rhs))
							return Unknown{};

						if (!is_int(rhs))
						{
							error(st, e.span, "operator || expects integers");
							return Unknown{};
						}
						return (std::get<std::int64_t>(rhs) != 0) ? 1 : 0;
					}

					if (op == ir::BinaryOp::concat)
					{
						auto join_ident = [](std::string_view a,
											 std::string_view b) -> std::string {
							std::string out;
							out.reserve(a.size() + b.size() + 1);
							out.append(a);

							if (!a.empty() && !b.empty())
							{
								auto const la = static_cast<unsigned char>(a.back());
								auto const rb = static_cast<unsigned char>(b.front());

								if (a.back() != '_' && b.front() != '_' && std::isalnum(la)
									&& std::isalnum(rb))
									out.push_back('_');
							}

							out.append(b);
							return out;
						};

						auto to_part = [&](auto&& self,
										   ir::Expr const& part) -> std::optional<std::string> {
							if (auto const* id = std::get_if<ir::ExprIdent>(&part.node))
							{
								if (!st.symbols.contains(id->name))
									return id->name;
							}

							if (auto const* cb = std::get_if<ir::ExprBinary>(&part.node))
							{
								if (cb->op == ir::BinaryOp::concat)
								{
									auto l = self(self, *cb->lhs);
									auto r = self(self, *cb->rhs);
									if (!l || !r)
										return std::nullopt;

									return join_ident(*l, *r);
								}
							}

							auto v = eval_value(st, part);
							if (is_unknown(v))
								return std::nullopt;

							if (!is_int(v) && !is_str(v))
							{
								error(st, part.span, "concat expects int or string operands");
								return std::nullopt;
							}

							return value_to_string(v);
						};

						auto lhs_s = to_part(to_part, *b.lhs);
						auto rhs_s = to_part(to_part, *b.rhs);
						if (!lhs_s || !rhs_s)
							return Unknown{};

						return join_ident(*lhs_s, *rhs_s);
					}

					auto lhs = eval_value(st, *b.lhs);
					auto rhs = eval_value(st, *b.rhs);

					if (is_unknown(lhs) || is_unknown(rhs))
						return Unknown{};

					if (op == ir::BinaryOp::eq || op == ir::BinaryOp::ne)
					{
						if (is_int(lhs) && is_int(rhs))
						{
							auto const a = std::get<std::int64_t>(lhs);
							auto const c = std::get<std::int64_t>(rhs);
							auto const eq = (a == c);
							return (op == ir::BinaryOp::eq) ? (eq ? 1 : 0) : (eq ? 0 : 1);
						}

						if (is_str(lhs) && is_str(rhs))
						{
							auto const& a = std::get<std::string>(lhs);
							auto const& c = std::get<std::string>(rhs);
							auto const eq = (a == c);
							return (op == ir::BinaryOp::eq) ? (eq ? 1 : 0) : (eq ? 0 : 1);
						}

						error(
							st,
							e.span,
							"==/!= requires both operands to be of the same type (int or string)");

						return Unknown{};
					}

					if (!is_int(lhs) || !is_int(rhs))
					{
						error(st, e.span, "binary operator expects integer operands");
						return Unknown{};
					}

					auto const a = std::get<std::int64_t>(lhs);
					auto const c = std::get<std::int64_t>(rhs);

					switch (op)
					{
						case ir::BinaryOp::add: {
							auto r = add_i64_checked(a, c);
							if (!r)
							{
								error(st, e.span, "integer overflow in +");
								return Unknown{};
							}
							return *r;
						}
						case ir::BinaryOp::sub: {
							auto r = sub_i64_checked(a, c);
							if (!r)
							{
								error(st, e.span, "integer overflow in -");
								return Unknown{};
							}
							return *r;
						}
						case ir::BinaryOp::mul: {
							auto r = mul_i64_checked(a, c);
							if (!r)
							{
								error(st, e.span, "integer overflow in *");
								return Unknown{};
							}
							return *r;
						}
						case ir::BinaryOp::div:
							if (c == 0)
							{
								error(st, e.span, "division by zero");
								return Unknown{};
							}
							if (a == std::numeric_limits<std::int64_t>::min() && c == -1)
							{
								error(st, e.span, "integer overflow in /");
								return Unknown{};
							}
							return a / c;
						case ir::BinaryOp::mod:
							if (c == 0)
							{
								error(st, e.span, "modulo by zero");
								return Unknown{};
							}
							return a % c;

						case ir::BinaryOp::shl:
						case ir::BinaryOp::shr: {
							if (c < 0 || c >= 64)
							{
								error(st, e.span, "shift count out of range");
								return Unknown{};
							}
							auto const ua = static_cast<std::uint64_t>(a);
							auto const sh = static_cast<std::uint64_t>(c);
							auto const r = (op == ir::BinaryOp::shl) ? (ua << sh) : (ua >> sh);
							return static_cast<std::int64_t>(r);
						}

						case ir::BinaryOp::bit_and:
							return a & c;
						case ir::BinaryOp::bit_or:
							return a | c;
						case ir::BinaryOp::bit_xor:
							return a ^ c;

						case ir::BinaryOp::lt:
							return (a < c) ? 1 : 0;
						case ir::BinaryOp::le:
							return (a <= c) ? 1 : 0;
						case ir::BinaryOp::gt:
							return (a > c) ? 1 : 0;
						case ir::BinaryOp::ge:
							return (a >= c) ? 1 : 0;

						default:
							break;
					}

					error(st, e.span, "unsupported binary operator in value mode");
					return Unknown{};
				},
			},
			e.node);
	}

	std::optional<std::string> Assembler::eval_name(PassState& st, ir::Expr const& e)
	{
		return std::visit(
			Overload{
				[&](ir::ExprIdent const& id) -> std::optional<std::string> { return id.name; },
				[&](ir::ExprInt const&) -> std::optional<std::string> { return std::nullopt; },
				[&](ir::ExprStr const&) -> std::optional<std::string> { return std::nullopt; },
				[&](ir::ExprBuiltin const&) -> std::optional<std::string> { return std::nullopt; },
				[&](ir::ExprUnary const& u) -> std::optional<std::string> {
					if (u.op != ir::UnaryOp::at)
						return std::nullopt;

					auto base = eval_name(st, *u.rhs);
					if (!base)
						return std::nullopt;

					auto it = st.symbols.find(*base);
					if (it != st.symbols.end() && it->second.kind == SymbolKind::symbolic
						&& !it->second.pointee.empty())
						return it->second.pointee;

					return *base;
				},
				[&](ir::ExprBinary const& b) -> std::optional<std::string> {
					if (b.op != ir::BinaryOp::concat)
						return std::nullopt;

					auto v = eval_value(st, e);
					if (is_unknown(v) || !is_str(v))
						return std::nullopt;

					return std::get<std::string>(v);
				},
			},
			e.node);
	}

	Assembler::Stream& Assembler::cur_stream(PassState& st)
	{
		auto it = st.streams.find(st.current_stream);
		if (it == st.streams.end())
		{
			auto [ins, _] =
				st.streams.emplace(st.current_stream, Stream{.name = st.current_stream});

			return ins->second;
		}
		return it->second;
	}

	void Assembler::emit_u8(PassState& st, std::uint8_t v)
	{
		auto& s = cur_stream(st);
		s.bytes.push_back(v);
		st.dollar_address += 1;
	}

	void Assembler::emit_u16(PassState& st, std::uint16_t v)
	{
		emit_u8(st, static_cast<std::uint8_t>(v & 255));
		emit_u8(st, static_cast<std::uint8_t>((v >> 8) & 255));
	}

	void Assembler::emit_u32(PassState& st, std::uint32_t v)
	{
		emit_u8(st, static_cast<std::uint8_t>(v & 255));
		emit_u8(st, static_cast<std::uint8_t>((v >> 8) & 255));
		emit_u8(st, static_cast<std::uint8_t>((v >> 16) & 255));
		emit_u8(st, static_cast<std::uint8_t>((v >> 24) & 255));
	}

	void Assembler::emit_u64(PassState& st, std::uint64_t v)
	{
		emit_u8(st, static_cast<std::uint8_t>(v & 255));
		emit_u8(st, static_cast<std::uint8_t>((v >> 8) & 255));
		emit_u8(st, static_cast<std::uint8_t>((v >> 16) & 255));
		emit_u8(st, static_cast<std::uint8_t>((v >> 24) & 255));
		emit_u8(st, static_cast<std::uint8_t>((v >> 32) & 255));
		emit_u8(st, static_cast<std::uint8_t>((v >> 40) & 255));
		emit_u8(st, static_cast<std::uint8_t>((v >> 48) & 255));
		emit_u8(st, static_cast<std::uint8_t>((v >> 56) & 255));
	}

	std::size_t Assembler::fingerprint(PassState const& st)
	{
		std::uint64_t h = 0;

		auto mix = [&](std::uint64_t x) {
			h ^= hash_u64(x + 0x9e3779b97f4a7c15 + (h << 6) + (h >> 2));
		};

		std::vector<std::string_view> sym_names;
		sym_names.reserve(st.symbols.size());
		for (auto const& [k, _] : st.symbols)
			sym_names.push_back(k);
		std::sort(sym_names.begin(), sym_names.end());

		for (auto name : sym_names)
		{
			auto const& sym = st.symbols.at(std::string(name));

			mix(fnv1a_str(name));
			mix(static_cast<std::uint64_t>(sym.kind));

			if (is_unknown(sym.value))
				mix(0x1111111111111111);
			else if (is_int(sym.value))
			{
				mix(0x2222222222222222);
				mix(static_cast<std::uint64_t>(std::get<std::int64_t>(sym.value)));
			}
			else if (is_str(sym.value))
			{
				mix(0x3333333333333333);
				mix(fnv1a_str(std::get<std::string>(sym.value)));
			}

			mix(fnv1a_str(sym.pointee));
		}

		std::vector<std::string_view> stream_names;
		stream_names.reserve(st.streams.size());
		for (auto const& [k, _] : st.streams)
			stream_names.push_back(k);

		std::sort(stream_names.begin(), stream_names.end());

		for (auto name : stream_names)
		{
			auto const& stream = st.streams.at(std::string(name));
			mix(fnv1a_str(name));
			mix(stream.origin);
			mix(fnv1a_bytes(stream.bytes));
		}

		mix(fnv1a_str(st.current_stream));
		mix(st.dollar_address);

		return static_cast<std::size_t>(h);
	}

	void Assembler::emit_diag(PassState& st, Diagnostic d)
	{
		++st.errors;

		if (!g_emit_diagnostics)
			return;

		m_diag->emit(d);
	}

	void Assembler::error(PassState& st, SourceSpan span, std::string msg)
	{
		Diagnostic d{};
		d.level = DiagnosticLevel::error;
		d.message = std::move(msg);
		d.primary = span;
		emit_diag(st, std::move(d));
	}

} // namespace yasme
