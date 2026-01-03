#ifndef YASME_ASM_ASSEMBLER_H
#define YASME_ASM_ASSEMBLER_H

#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>
#include <yasme/Diagnostics.hh>
#include <yasme/ir/Yir.hh>
#include <yasme/support/SourceManager.hh>
#include <yasme/support/Span.hh>

namespace yasme
{
	struct AssembleOptions
	{
		std::size_t max_passes{100};
		bool error_on_unresolved{true};
		bool run_final_postpone{true};
	};

	struct AssembleOutput
	{
		std::vector<std::uint8_t> bytes{};
		std::size_t passes{};
		std::size_t errors{};
	};

	class Assembler
	{
	public:
		explicit Assembler(Diagnostics& diag) noexcept;

		AssembleOutput assemble(ir::Program const& program, AssembleOptions opt = {});

		struct Unknown
		{
		};

		using Value = std::variant<Unknown, std::int64_t, std::string>;

		[[nodiscard]] static bool is_unknown(Value const& v) noexcept
		{
			return std::holds_alternative<Unknown>(v);
		}

		[[nodiscard]] static bool is_int(Value const& v) noexcept
		{
			return std::holds_alternative<std::int64_t>(v);
		}

		[[nodiscard]] static bool is_str(Value const& v) noexcept
		{
			return std::holds_alternative<std::string>(v);
		}

		enum class SymbolKind
		{
			numeric,
			string,
			symbolic,
			label,

			define_numeric,
			define_string,
		};

		struct Symbol
		{
			SymbolKind kind{SymbolKind::numeric};
			Value value{Unknown{}};
			std::string pointee{};
			SourceSpan declared_at{};
		};

		struct Stream
		{
			std::string name{};
			std::vector<std::uint8_t> bytes{};
			std::uint64_t origin{};
		};

		struct PassState
		{
			std::unordered_map<std::string, Symbol> symbols{};

			std::unordered_map<std::string, Stream> streams{};

			std::uint64_t dollar_address{};
			std::string current_stream{"output"};

			std::vector<ir::StmtPostpone const*> postpone_each_pass{};
			std::vector<ir::StmtPostpone const*> postpone_after_stable{};

			std::size_t errors{};
		};

		[[nodiscard]] static PassState seed_next_pass(PassState const& prev)
		{
			PassState st{};
			st.symbols = prev.symbols;

			st.streams.emplace("output", Stream{.name = "output"});
			st.current_stream = "output";
			st.dollar_address = 0;

			return st;
		}

		void error(PassState& st, SourceSpan span, std::string msg);

	private:
		[[nodiscard]] PassState run_pass(ir::Program const& program, PassState const& seed);

		void walk_stmt(PassState& st, ir::Stmt const& stmt);
		void walk_block(PassState& st, std::vector<ir::StmtPtr> const& body);

		void run_postpone_each_pass(PassState& st);
		void run_postpone_after_stable(PassState& st);

		void apply_org(PassState& st, ir::StmtOrg const& s);
		void apply_label(PassState& st, ir::StmtLabel const& s);
		void apply_assign(PassState& st, ir::StmtAssign const& s);
		void apply_define(PassState& st, ir::StmtDefine const& s);
		void apply_emit_data(PassState& st, ir::StmtEmitData const& s);
		void apply_virtual(PassState& st, ir::StmtVirtual const& s);
		void apply_postpone(PassState& st, ir::StmtPostpone const& s);

		[[nodiscard]] Value eval_value(PassState& st, ir::Expr const& e);
		[[nodiscard]] std::optional<std::string> eval_name(PassState& st, ir::Expr const& e);

		[[nodiscard]] Value eval_builtin(PassState& st, ir::ExprBuiltin const& b);

		[[nodiscard]] Stream& cur_stream(PassState& st);

		void emit_u8(PassState& st, std::uint8_t v);
		void emit_u16(PassState& st, std::uint16_t v);
		void emit_u32(PassState& st, std::uint32_t v);
		void emit_u64(PassState& st, std::uint64_t v);

		[[nodiscard]] static std::size_t fingerprint(PassState const& st);

		Diagnostics* m_diag{};
	};

} // namespace yasme

#endif /* YASME_ASM_ASSEMBLER_H */
