#ifndef A1DF5696F9FA459A9AC38015A76EC879
#define A1DF5696F9FA459A9AC38015A76EC879
#ifdef A1DF5696F9FA459A9AC38015A76EC879

#include "zero_cost_serialization/serializable.h"
#include "zero_cost_serialization/reinterpret_memory.h"

#include <algorithm>
#include <concepts>
#include <functional>
#include <span>
#include <tuple>
#include <type_traits>
#include <utility>

namespace zero_cost_serialization {
	namespace detail {
		template <typename T>
		constexpr auto check_size(bool& more, std::size_t& size) noexcept
		{
			if constexpr (std::is_unbounded_array_v<T>)
				more = false;
			else if (more)
				size += sizeof(T);
		}

		template <typename... Ts>
		[[nodiscard]] constexpr auto required_size() noexcept
		{
			[[maybe_unused]] auto more = true;
			auto rsize = std::size_t{};
			(check_size<Ts>(more, rsize), ...);
			return rsize;
		}

		template <typename T>
		[[nodiscard]] auto unpack_element(std::span<std::byte>& data) noexcept
		{
			using E = std::conditional_t<std::is_unbounded_array_v<T>, std::remove_extent_t<T>, T>;
			auto sz = std::is_unbounded_array_v<T> ? data.size() / sizeof(E) * sizeof(E) : sizeof(E);
			auto p = data;
			data = data.subspan(sz);
			if constexpr (std::is_unbounded_array_v<T>) {
				if (sz) //unsafe cast. we cast back to E* before use.
					return reinterpret_cast<T*>(zero_cost_serialization::reinterpret_memory<E>(p.data(), sz));
				else
					return static_cast<T*>(nullptr);
			} else
				return zero_cost_serialization::reinterpret_memory<E>(p.data(), sz);
		}

		template <typename T>
		[[nodiscard]] constexpr decltype(auto) repack_element(T* t) noexcept
		{
			using E = std::remove_extent_t<T>;
			if constexpr (std::is_unbounded_array_v<T>) return reinterpret_cast<E*>(t);
			else return *t;
		}

		template <typename F, typename Args, typename Tuple>
		struct noexcept_test : std::false_type {};

		template <typename F, typename Args, typename... Ts>
		[[nodiscard]] constexpr auto noexcept_test_helper() noexcept
		{
			if constexpr (sizeof...(Ts)) {
				return [] <std::size_t... Is>(const std::index_sequence<Is...>&) {
					std::tuple<Ts*...> ptrs;

					using A = std::remove_pointer_t<std::tuple_element_t<sizeof...(Ts) - 1, decltype(ptrs)>>;

					using Tuple = std::conditional_t<std::is_unbounded_array_v<A>,
						decltype(std::tuple_cat(std::declval<Args>(), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...), std::forward_as_tuple(std::declval<std::size_t&>()))),
						decltype(std::tuple_cat(std::declval<Args>(), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...)))>;

					return noexcept(std::apply(std::declval<F&>(), std::declval<Tuple>()));
				}(std::index_sequence_for<Ts...>());
			}
			else if constexpr (std::tuple_size_v<std::remove_reference_t<Args>>)
				return noexcept(std::apply(std::declval<F&>(), std::declval<Args>()));
			else
				return std::is_nothrow_invocable_v<F>;
		}

		template <typename F, typename Args, typename... Ts>
		struct noexcept_test<F, Args, std::tuple<Ts...>>
		{
			static constexpr auto value = noexcept_test_helper<F, Args, Ts...>();
		};

		template <typename F, typename Args, typename... Ts>
		inline constexpr auto noexcept_test_v = noexcept_test<F, Args, std::tuple<Ts...>>::value;

		template <typename F, typename Args, typename... Ts, std::size_t... Is>
		requires zero_cost_serialization::is_serializable_v<Ts...>
		and ((not std::is_unbounded_array_v<std::tuple_element_t<Is, std::tuple<Ts...>>> or 1 + Is == sizeof...(Ts)) and ... and true)
		[[nodiscard]] decltype(auto) invoke(const std::index_sequence<Is...>&, F&& f, Args&& args, std::span<std::byte> data) noexcept(noexcept_test_v<F, Args, Ts...>)
		{
			if constexpr (sizeof...(Ts)) {

				std::tuple<Ts*...> ptrs;
				
				using A = std::remove_pointer_t<std::tuple_element_t<sizeof...(Ts) - 1, decltype(ptrs)>>;

				using Tuple = std::conditional_t<std::is_unbounded_array_v<A>,
					decltype(std::tuple_cat(std::forward<Args>(args), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...), std::forward_as_tuple(std::declval<std::size_t&>()))),
					decltype(std::tuple_cat(std::forward<Args>(args), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...)))>;

				using R = std::decay_t<decltype(std::apply(std::forward<F>(f), std::declval<Tuple>()))>;

				auto copy = data;
				((std::get<Is>(ptrs) = unpack_element<Ts>(copy)), ...);
				if constexpr (std::is_unbounded_array_v<A>) {
					auto n = (data.size() - required_size<Ts...>()) / sizeof(std::remove_extent_t<A>);
					if constexpr (std::is_void_v<R>) std::apply(std::forward<F>(f), std::tuple_cat(std::forward<Args>(args), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...), std::forward_as_tuple(n)));
					else return std::apply(std::forward<F>(f), std::tuple_cat(std::forward<Args>(args), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...), std::forward_as_tuple(n)));
				} else {
					if constexpr (std::is_void_v<R>) std::apply(std::forward<F>(f), std::tuple_cat(std::forward<Args>(args), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...)));
					else return std::apply(std::forward<F>(f), std::tuple_cat(std::forward<Args>(args), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...)));
				}
			} else {
				if constexpr (std::is_void_v<decltype(std::apply(std::forward<F>(f), std::forward<Args>(args)))>)
					std::apply(std::forward<F>(f), std::forward<Args>(args));
				else 
					return std::apply(std::forward<F>(f), std::forward<Args>(args));
			}
		}
		template <zero_cost_serialization::detail::reflectable_class T>
		using tuple_of_refs = decltype(zero_cost_serialization::detail::make_tuple<T, std::integral_constant<std::size_t, zero_cost_serialization::detail::field_count<T>()>>{}(std::declval<T&>()));

		template <typename T, std::size_t... Is>
		[[nodiscard]] auto tuple_of_refs_flex_helper(const T&, const std::index_sequence<Is...>&) noexcept
		{
			using t = std::tuple<std::conditional_t<Is + 1 == std::tuple_size_v<T>,
				std::add_rvalue_reference_t<std::remove_extent_t<std::remove_reference_t<std::tuple_element_t<Is, T>>>[]>,
				std::tuple_element_t<Is, T>>...>;
			return static_cast<t*>(nullptr);
		}

		template <zero_cost_serialization::detail::reflectable_class T>
		using tuple_of_refs_flex = std::remove_pointer_t<decltype(tuple_of_refs_flex_helper(std::declval<tuple_of_refs<T>>(), std::make_index_sequence<std::tuple_size_v<tuple_of_refs<T>>>()))>;

		template <typename F, typename T, typename Args, typename = void>
		struct is_unpack_invocable : std::false_type {};

		template <typename F, typename T, typename Args, typename = void>
		struct is_unpack_invocable_flex : std::false_type {};

		template <typename T>
		concept empty_class = requires
		{
			requires std::is_empty_v<T>;
			requires std::is_aggregate_v<T>;
			requires std::is_trivial_v<T>;
			requires std::is_standard_layout_v<T>;
		};

		template <typename Args, typename T>
		struct unpack_invocable_tuple {};

		template <typename Args, zero_cost_serialization::detail::reflectable_class T>
		struct unpack_invocable_tuple<Args, T>
		{
			using type = decltype(std::tuple_cat(std::declval<Args>(), std::declval<detail::tuple_of_refs<T>>()));
		};

		template <typename Args, empty_class T>
		struct unpack_invocable_tuple<Args, T>
		{
			using type = decltype(std::tuple_cat(std::declval<Args>()));
		};


		template <typename T>
		concept reflectable_or_empty = empty_class<T> or zero_cost_serialization::detail::reflectable_class<T>;

		template <typename Args, reflectable_or_empty T>
		using unpack_invocable_tuple_type = unpack_invocable_tuple<Args, T>::type;

		template <typename Args, zero_cost_serialization::detail::reflectable_class T>
		struct unpack_invocable_flex_tuple
		{
			using type = decltype(std::tuple_cat(std::declval<Args>(), std::declval<detail::tuple_of_refs_flex<T>>(), std::forward_as_tuple(std::declval<std::size_t>())));
		};

		template <typename Args, zero_cost_serialization::detail::reflectable_class T>
		using unpack_invocable_flex_tuple_type = unpack_invocable_flex_tuple<Args, T>::type;

		template <typename F, typename... Ts>
		struct unpack_invokable_args : std::false_type {};

		template <typename F, typename... Ts>
		struct unpack_invokable_args<F, std::tuple<Ts...>> : std::is_invocable<F, Ts...> {};

		template <typename F, typename T>
		inline constexpr auto unpack_invokable_args_v = unpack_invokable_args<F, T>::value;

		template <typename F, zero_cost_serialization::detail::reflectable_class T, typename Args>
		struct is_unpack_invocable<F, T, Args, std::void_t<unpack_invocable_tuple_type<Args, T>>>
		{
			static constexpr auto value = unpack_invokable_args_v<F, unpack_invocable_tuple_type<Args, T>>;
		};

		template <typename F, empty_class T, typename Args>
		struct is_unpack_invocable<F, T, Args, std::void_t<decltype(std::tuple_cat(std::declval<Args>()))>>
		{
			static constexpr auto value = unpack_invokable_args_v<F, Args>;
		};

		template <typename F, typename T, typename Args = std::tuple<>>
		inline constexpr auto is_unpack_invocable_v = is_unpack_invocable<F, T, Args>::value;

		template <typename F, zero_cost_serialization::detail::reflectable_class T, typename Args>
		struct is_unpack_invocable_flex<F, T, Args, std::void_t<unpack_invocable_flex_tuple_type<Args, T>>>
		{
			static constexpr auto value = unpack_invokable_args_v<F, unpack_invocable_flex_tuple_type<Args, T>>;
		};

		template <typename F, typename T, typename Args = std::tuple<>>
		inline constexpr auto is_unpack_invocable_flex_v = is_unpack_invocable_flex<F, T, Args>::value;

		template <typename... Ts>
		requires zero_cost_serialization::is_serializable_v<Ts...>
		[[nodiscard]] decltype(auto) invoke(auto&& f, auto&& args, std::span<std::byte> data) noexcept(noexcept(detail::invoke<decltype(f), decltype(args), Ts...>(std::index_sequence_for<Ts...>(), std::forward<decltype(f)>(f), std::forward<decltype(args)>(args), data)))
		{
			if (data.size() < required_size<Ts...>()) [[unlikely]] {
				alignas(Ts...) std::array<std::byte, required_size<Ts...>()> temp;
				std::ranges::copy(data, temp.data());
				std::ranges::fill(std::span(temp).subspan(sizeof(temp) - data.size()), std::byte{});
				return detail::invoke<decltype(f), decltype(args), Ts...>(std::index_sequence_for<Ts...>(), std::forward<decltype(f)>(f), std::forward<decltype(args)>(args), temp);
			}
			else
				return detail::invoke<decltype(f), decltype(args), Ts...>(std::index_sequence_for<Ts...>(), std::forward<decltype(f)>(f), std::forward<decltype(args)>(args), data);
		}

	}

	template <typename... Ts>
	inline constexpr auto invoke_size_v = detail::required_size<Ts...>();

	template <typename... Ts>
	requires zero_cost_serialization::is_serializable_v<Ts...>
	decltype(auto) invoke(auto&& f, std::span<std::byte> data) noexcept(noexcept(detail::invoke<Ts...>(std::forward<decltype(f)>(f), std::make_tuple(), data)))
	{
		return detail::invoke<Ts...>(std::forward<decltype(f)>(f), std::make_tuple(), data);
	}
	template <typename... Ts>
	requires zero_cost_serialization::is_serializable_v<Ts...>
	decltype(auto) invoke(auto&& f, auto&& args, std::span<std::byte> data) noexcept(noexcept(detail::invoke<Ts...>(std::forward<decltype(f)>(f), std::forward<decltype(args)>(args), data)))
	{
		return detail::invoke<Ts...>(std::forward<decltype(f)>(f), std::forward<decltype(args)>(args), data);
	}

	template <typename... Ts, std::size_t N>
	requires (zero_cost_serialization::is_serializable_v<Ts...>
	and N >= invoke_size_v<Ts...>)
	decltype(auto) invoke(auto&& f, std::byte(&data)[N]) noexcept(noexcept(detail::invoke<decltype(f), std::tuple<>, Ts...>(std::index_sequence_for<Ts...>(), std::forward<decltype(f)>(f), std::make_tuple(), std::span<std::byte, N>(data))))
	{
		return detail::invoke<decltype(f), std::tuple<>, Ts...>(std::index_sequence_for<Ts...>(), std::forward<decltype(f)>(f), std::make_tuple(), std::span(data));
	}

	template <typename... Ts, std::size_t N>
	requires (zero_cost_serialization::is_serializable_v<Ts...>
	and N >= invoke_size_v<Ts...>)
	decltype(auto) invoke(auto&& f, auto&& args, std::byte(&data)[N]) noexcept(noexcept(detail::invoke<decltype(f), decltype(args), Ts...>(std::index_sequence_for<Ts...>(), std::forward<decltype(f)>(f), std::forward<decltype(args)>(args), std::span<std::byte, N>(data))))
	{
		return detail::invoke<decltype(f), decltype(args), Ts...>(std::index_sequence_for<Ts...>(), std::forward<decltype(f)>(f), std::forward<decltype(args)>(args), std::span(data));
	}
}
#endif
#endif
