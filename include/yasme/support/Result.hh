#ifndef YASME_SUPPORT_RESULT_HH
#define YASME_SUPPORT_RESULT_HH

#include <concepts>
#include <exception>
#include <functional>
#include <memory>
#include <type_traits>
#include <utility>

namespace yasme
{
	namespace detail
	{
		template <class F, class V>
		concept value_factory =
			std::invocable<F> && std::convertible_to<std::invoke_result_t<F>, V>;
	} // namespace detail

	template <class V, class E> class Result
	{
	public:
		Result(V value) noexcept(std::is_nothrow_move_constructible_v<V>) : m_ok(true)
		{
			std::construct_at(std::addressof(m_val), std::move(value));
		}

		Result(E err) noexcept(std::is_nothrow_move_constructible_v<E>) : m_ok(false)
		{
			std::construct_at(std::addressof(m_err), std::move(err));
		}

		Result(Result const& other)
			requires(std::copy_constructible<V> && std::copy_constructible<E>)
			: m_ok(other.m_ok)
		{
			if (m_ok)
				std::construct_at(std::addressof(m_val), other.m_val);
			else
				std::construct_at(std::addressof(m_err), other.m_err);
		}

		Result(Result&& other) noexcept(std::is_nothrow_move_constructible_v<V>
										&& std::is_nothrow_move_constructible_v<E>)
			: m_ok(other.m_ok)
		{
			if (m_ok)
				std::construct_at(std::addressof(m_val), std::move(other.m_val));
			else
				std::construct_at(std::addressof(m_err), std::move(other.m_err));
		}

		Result& operator=(Result const&) = delete;
		Result& operator=(Result&&) = delete;

		~Result()
		{
			if (m_ok)
				std::destroy_at(std::addressof(m_val));
			else
				std::destroy_at(std::addressof(m_err));
		}

		bool ok() const noexcept { return m_ok; }

		V& value() & noexcept
		{
			if (!m_ok)
				std::terminate();
			return m_val;
		}

		V const& value() const& noexcept
		{
			if (!m_ok)
				std::terminate();
			return m_val;
		}

		E& err() & noexcept
		{
			if (m_ok)
				std::terminate();
			return m_err;
		}

		E const& err() const& noexcept
		{
			if (m_ok)
				std::terminate();
			return m_err;
		}

		template <typename F>
			requires detail::value_factory<F, V>
		V value_else(F f) const noexcept
		{
			if (m_ok)
				return m_val;
			return static_cast<V>(std::invoke(std::move(f)));
		}

		template <typename U>
			requires std::constructible_from<V, U>
		V value_or(U&& v) const noexcept
		{
			if (m_ok)
				return m_val;
			return V(std::forward<U>(v));
		}

	private:
		union
		{
			E m_err;
			V m_val;
		};
		bool m_ok;
	};

} // namespace yasme

#endif /* YASME_SUPPORT_RESULT_HH */
