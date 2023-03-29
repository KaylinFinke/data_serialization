#if not defined(B0B29471F26C4810B8D8F3B0EBAE97AD)
#define B0B29471F26C4810B8D8F3B0EBAE97AD
#if defined(B0B29471F26C4810B8D8F3B0EBAE97AD)

#include "invoke.h"
#include "transparently_serializable.h"

#include <cstddef>
#include <tuple>
#include <type_traits>
#include <utility>

namespace data_serialization {
	namespace detail {
		template <typename F, typename Args, typename T, std::size_t... Is>
		[[nodiscard]] decltype(auto) apply(const std::index_sequence<Is...>&, F&& f, Args&& args, std::byte* data, std::size_t size)
		{
			return data_serialization::invoke<std::remove_reference_t<std::tuple_element_t<Is, T>>...>(std::forward<F>(f), std::forward<Args>(args), data, size);
		}

		template <typename T, std::size_t... Is>
		[[nodiscard]] consteval auto invoke_size_pack(const std::index_sequence<Is...>&) noexcept
		{
			return invoke_size_v<std::remove_reference_t<std::tuple_element_t<Is, T>>...>;
		}

		template <typename T, typename F, typename Args>
		requires is_unpack_invocable_v<F, T, Args>
		[[nodiscard]] consteval auto apply_size() noexcept
		{
			if constexpr (empty_class<T>) {
				return invoke_size_pack<std::tuple<>>(std::make_index_sequence<0>());
			} else {
				using TT = tuple_of_refs<T>;
				return invoke_size_pack<TT>(std::make_index_sequence<std::tuple_size_v<TT>>());
			}
		}
		template <typename T, typename F, typename Args>
		requires is_unpack_invocable_flex_v<F, T, Args>
		[[nodiscard]] consteval auto apply_size() noexcept
		{
			using TT = tuple_of_refs_flex<T>;
			return invoke_size_pack<TT>(std::make_index_sequence<std::tuple_size_v<TT>>());
		}

		template <typename T, typename F, typename Args>
		[[nodiscard]] consteval auto flex_element_size() noexcept
		{
			if constexpr (is_unpack_invocable_flex_v<F, T, Args>) {
				using TT = tuple_of_refs_flex<T>;
				return sizeof(std::remove_extent_t<std::remove_reference_t<std::tuple_element_t<std::tuple_size_v<TT> -1, TT>>>);
			}
			return std::size_t{};
		}
	}

	template <typename T, typename F, typename Args = std::tuple<>>
	inline constexpr auto apply_size_v = detail::apply_size<T, F, Args>();

	template <common_platform::detail::reflectable_class T, typename F, typename Args = std::tuple<>>
	inline constexpr auto flex_element_size_v = detail::flex_element_size<T, F, Args>();

	template <detail::empty_class T, typename F>
	requires detail::is_unpack_invocable_v<F, T>
	[[nodiscard]] decltype(auto) apply(F&& f, std::byte* data, std::size_t size)
	{
		return detail::apply<F, std::tuple<>, std::tuple<>>(std::make_index_sequence<0>(), std::forward<F>(f), std::make_tuple(), data, size);
	}

	template <detail::empty_class T, typename Args, typename F>
	requires detail::is_unpack_invocable_v<F, T, Args>
	[[nodiscard]] decltype(auto) apply(F&& f, Args&& args, std::byte* data, std::size_t size)
	{
		return detail::apply<F, Args, std::tuple<>>(std::make_index_sequence<0>(), std::forward<F>(f), std::forward<Args>(args), data, size);
	}

	template <common_platform::detail::reflectable_class T, typename F>
	requires detail::is_unpack_invocable_v<F, T>
	[[nodiscard]] decltype(auto) apply(F&& f, std::byte* data, std::size_t size)
	{
		using TT = detail::tuple_of_refs<T>;
		return detail::apply<F, std::tuple<>, TT>(std::make_index_sequence<std::tuple_size_v<TT>>(), std::forward<F>(f), std::make_tuple(), data, size);
	}

	template <common_platform::detail::reflectable_class T, typename Args, typename F>
	requires detail::is_unpack_invocable_v<F, T, Args>
	[[nodiscard]] decltype(auto) apply(F&& f, Args&& args, std::byte* data, std::size_t size)
	{
		using TT = detail::tuple_of_refs<T>;
		return detail::apply<F, Args, TT>(std::make_index_sequence<std::tuple_size_v<TT>>(), std::forward<F>(f), std::forward<Args>(args), data, size);
	}

	template <common_platform::detail::reflectable_class T, typename F>
	requires detail::is_unpack_invocable_flex_v<F, T>
	[[nodiscard]] decltype(auto) apply(F&& f, std::byte* data, std::size_t size)
	{
		using TT = detail::tuple_of_refs_flex<T>;
		return detail::apply<F, std::tuple<>, TT>(std::make_index_sequence<std::tuple_size_v<TT>>(), std::forward<F>(f), std::make_tuple(), data, size);
	}

	template <common_platform::detail::reflectable_class T, typename Args, typename F>
	requires detail::is_unpack_invocable_flex_v<F, T, Args>
	[[nodiscard]] decltype(auto) apply(F&& f, Args&& args, std::byte* data, std::size_t size)
	{
		using TT = detail::tuple_of_refs_flex<T>;
		return detail::apply<F, Args, TT>(std::make_index_sequence<std::tuple_size_v<TT>>(), std::forward<F>(f), std::forward<Args>(args), data, size);
	}
}
#endif
#endif
