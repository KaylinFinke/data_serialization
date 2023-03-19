#if not defined(A1DF5696F9FA459A9AC38015A76EC879)
#define A1DF5696F9FA459A9AC38015A76EC879
#if defined(A1DF5696F9FA459A9AC38015A76EC879)

#include "transparently_serializable.h"
#include "reinterpret_memory.h"

#include <concepts>
#include <tuple>
#include <type_traits>
#include <utility>

namespace data_serialization {
	namespace detail {
		template <typename T>
		constexpr auto check_size(std::same_as<bool> auto& more, std::size_t& size) noexcept
		{
			if constexpr (std::is_unbounded_array_v<T>)
				more = false;
			else if (more)
				size += sizeof(T);
		}

		template <typename... Ts>
		[[nodiscard]] constexpr auto required_size() noexcept
		{
			auto more = true;
			auto rsize = std::size_t{};
			(check_size<Ts>(more, rsize), ...);
			return rsize;
		}

		template <typename T>
		[[nodiscard]] auto unpack_element(std::byte*& data, std::size_t& size) noexcept
		{
			using E = std::conditional_t<std::is_unbounded_array_v<T>, std::remove_extent_t<T>, T>;
			auto sz = std::is_unbounded_array_v<T> ? size / sizeof(E) * sizeof(E) : sizeof(E);
			data += sz;
			size -= sz;
			if constexpr (std::is_unbounded_array_v<T>) {
				if (sz) //unsafe cast. we cast back to E* before use.
					return reinterpret_cast<T*>(type_conversion::reinterpret_memory<E>(data - sz, sz));
				else
					return static_cast<T*>(nullptr);
			} else
				return type_conversion::reinterpret_memory<E>(data - sz, sz);
		}

		template <typename T>
		[[nodiscard]] constexpr decltype(auto) repack_element(T* t) noexcept
		{
			using E = std::remove_extent_t<T>;
			if constexpr (std::is_unbounded_array_v<T>) return reinterpret_cast<E*>(t);
			else return *t;
		}

		template <typename F, typename Args, typename Tuple>
		struct noexcept_test final : std::false_type {};

		template <typename F, typename Args, typename... Ts>
		struct noexcept_test<F, Args, std::tuple<Ts...>> final
		{
			static constexpr auto value = [] <std::size_t... Is>(const std::index_sequence<Is...>&) {
				std::tuple<Ts*...> ptrs;

				using A = std::remove_pointer_t<std::tuple_element_t<sizeof...(Ts) - 1, decltype(ptrs)>>;

				using Tuple = std::conditional_t<std::is_unbounded_array_v<A>,
					decltype(std::tuple_cat(std::declval<Args>(), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...), std::forward_as_tuple(std::declval<std::size_t&>()))),
					decltype(std::tuple_cat(std::declval<Args>(), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...)))>;

				return noexcept(std::apply(std::declval<F&>(), std::declval<Tuple>()));
			}(std::index_sequence_for<Ts...>());
		};

		template <typename F, typename Args, typename... Ts>
		inline constexpr auto noexcept_test_v = noexcept_test<F, Args, std::tuple<Ts...>>::value;

		template <typename F, typename Args, typename... Ts, std::size_t... Is>
		requires common_platform::is_transparently_serializable_v<Ts...>
		and ((not std::is_unbounded_array_v<std::tuple_element_t<Is, std::tuple<Ts...>>> or 1 + Is == sizeof...(Ts)) and ... and true)
		[[nodiscard]] decltype(auto) invoke(const std::index_sequence<Is...>&, F&& f, Args&& args, std::byte* data, std::size_t size) noexcept(noexcept_test_v<F, Args, Ts...>)
		{
			if constexpr (sizeof...(Ts)) {

				std::tuple<Ts*...> ptrs;
				
				using A = std::remove_pointer_t<std::tuple_element_t<sizeof...(Ts) - 1, decltype(ptrs)>>;

				using Tuple = std::conditional_t<std::is_unbounded_array_v<A>,
					decltype(std::tuple_cat(std::forward<Args>(args), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...), std::forward_as_tuple(std::declval<std::size_t&>()))),
					decltype(std::tuple_cat(std::forward<Args>(args), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...)))>;

				using R = std::decay_t<decltype(std::apply(std::forward<F>(f), std::declval<Tuple>()))>;

				auto remaining = size;
				((std::get<Is>(ptrs) = unpack_element<Ts>(data, remaining)), ...);
				if constexpr (std::is_unbounded_array_v<A>) {
					auto n = (size - required_size<Ts...>()) / sizeof(std::remove_extent_t<A>);
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
		template <common_platform::detail::reflectable_class T>
		using tuple_of_refs = decltype(common_platform::detail::make_tuple<T, std::integral_constant<std::size_t, common_platform::detail::field_count<T>()>>{}(std::declval<T&>()));

		template <typename T, std::size_t... Is>
		[[nodiscard]] auto tuple_of_refs_flex_helper(const T&, const std::index_sequence<Is...>&) noexcept
		{
			using t = std::tuple<std::conditional_t<Is + 1 == std::tuple_size_v<T>,
				std::add_rvalue_reference_t<std::remove_extent_t<std::remove_reference_t<std::tuple_element_t<Is, T>>>[]>,
				std::tuple_element_t<Is, T>>...>;
			return static_cast<t*>(nullptr);
		}

		template <common_platform::detail::reflectable_class T>
		using tuple_of_refs_flex = std::remove_pointer_t<decltype(tuple_of_refs_flex_helper(std::declval<tuple_of_refs<T>>(), std::make_index_sequence<std::tuple_size_v<tuple_of_refs<T>>>()))>;

		template <typename F, typename T, typename Args, typename = void>
		struct is_unpack_invocable : std::false_type {};

		template <typename F, typename T, typename Args, typename = void>
		struct is_unpack_invocable_flex : std::false_type {};

		template <typename Args, common_platform::detail::reflectable_class T>
		struct unpack_invocable_tuple
		{
			using type = decltype(std::tuple_cat(std::declval<Args>(), std::declval<detail::tuple_of_refs<T>>()));
		};

		template <typename Args, common_platform::detail::reflectable_class T>
		using unpack_invocable_tuple_type = unpack_invocable_tuple<Args, T>::type;

		template <typename Args, common_platform::detail::reflectable_class T>
		struct unpack_invocable_flex_tuple
		{
			using type = decltype(std::tuple_cat(std::declval<Args>(), std::declval<detail::tuple_of_refs_flex<T>>(), std::forward_as_tuple(std::declval<std::size_t>())));
		};

		template <typename Args, common_platform::detail::reflectable_class T>
		using unpack_invocable_flex_tuple_type = unpack_invocable_flex_tuple<Args, T>::type;

		template <typename F, typename... Ts>
		struct unpack_invokable_args : std::false_type {};

		template <typename F, typename... Ts>
		struct unpack_invokable_args<F, std::tuple<Ts...>> : std::is_invocable<F, Ts...> {};

		template <typename F, typename T>
		inline constexpr auto unpack_invokable_args_v = unpack_invokable_args<F, T>::value;

		template <typename F, common_platform::detail::reflectable_class T, typename Args>
		struct is_unpack_invocable<F, T, Args, std::void_t<unpack_invocable_tuple_type<Args, T>>> final
		{
			static constexpr auto value = unpack_invokable_args_v<F, unpack_invocable_tuple_type<Args, T>>;
		};

		template <typename F, common_platform::detail::reflectable_class T, typename Args = std::tuple<>>
		inline constexpr auto is_unpack_invocable_v = is_unpack_invocable<F, T, Args>::value;

		template <typename F, common_platform::detail::reflectable_class T, typename Args>
		struct is_unpack_invocable_flex<F, T, Args, std::void_t<unpack_invocable_flex_tuple_type<Args, T>>> final
		{
			static constexpr auto value = unpack_invokable_args_v<F, unpack_invocable_flex_tuple_type<Args, T>>;
		};

		template <typename F, common_platform::detail::reflectable_class T, typename Args = std::tuple<>>
		inline constexpr auto is_unpack_invocable_flex_v = is_unpack_invocable_flex<F, T, Args>::value;

		template <typename... Ts>
		requires (common_platform::is_transparently_serializable_v<Ts...>
		and common_platform::is_common_platform)
		[[nodiscard]] decltype(auto) invoke(auto&& f, auto&& args, std::byte* data, std::size_t size)
		{
			alignas(Ts...) std::byte temp[required_size<Ts...>()];
			using F = std::remove_reference_t<decltype(f)>;
			using Args = std::remove_reference_t<decltype(args)>;

			if (size < sizeof(temp)) [[likely]]
				std::memcpy(temp, data, size);
			if (size < sizeof(temp)) [[likely]]
				std::memset(temp + size, 0, (sizeof(temp) - size));
			return detail::invoke<F, Args, Ts...>(std::index_sequence_for<Ts...>(), std::forward<F>(f), std::forward<Args>(args), temp, sizeof(temp));
		}

	}
	template <typename... Ts>
	requires (common_platform::is_transparently_serializable_v<Ts...>
	and common_platform::is_common_platform)
	[[nodiscard]] decltype(auto) invoke(auto&& f, std::byte* data, std::size_t size)
	{
		using F = std::remove_reference_t<decltype(f)>;
		if (size >= detail::required_size<Ts...>()) [[likely]]
			return detail::invoke<F, std::tuple<>, Ts...>(std::index_sequence_for<Ts...>(), std::forward<F>(f), std::make_tuple(), data, size);
		else
			return detail::invoke<Ts...>(std::forward<F>(f), std::make_tuple(), data, size);
	}
	template <typename... Ts>
	requires (common_platform::is_transparently_serializable_v<Ts...>
	and common_platform::is_common_platform)
	[[nodiscard]] decltype(auto) invoke(auto&& f, auto&& args, std::byte* data, std::size_t size)
	{
		using F = std::remove_reference_t<decltype(f)>;
		using Args = std::remove_reference_t<decltype(args)>;
		if (size >= detail::required_size<Ts...>()) [[likely]]
			return detail::invoke<F, Args, Ts...>(std::index_sequence_for<Ts...>(), std::forward<F>(f), std::forward<Args>(args), data, size);
		else
			return detail::invoke<Ts...>(std::forward<F>(f), std::forward<Args>(args), data, size);
	}

	template <typename... Ts, std::size_t N>
	requires (common_platform::is_transparently_serializable_v<Ts...>
	and common_platform::is_common_platform
	and N >= detail::required_size<Ts...>())
	[[nodiscard]] decltype(auto) invoke(auto&& f, std::byte(&data)[N])
	{
		using F = std::remove_reference_t<decltype(f)>;
		return detail::invoke<F, std::tuple<>, Ts...>(std::index_sequence_for<Ts...>(), std::forward<F>(f), std::make_tuple(), data, sizeof(data));
	}

	template <typename... Ts, std::size_t N>
	requires (common_platform::is_transparently_serializable_v<Ts...>
	and common_platform::is_common_platform
	and N >= detail::required_size<Ts...>())
	[[nodiscard]] decltype(auto) invoke(auto&& f, auto&& args, std::byte(&data)[N])
	{
		using F = std::remove_reference_t<decltype(f)>;
		using Args = std::remove_reference_t<decltype(args)>;
		return detail::invoke<F, Args, Ts...>(std::index_sequence_for<Ts...>(), std::forward<F>(f), std::forward<Args>(args), data, sizeof(data));
	}
}
#endif
#endif
