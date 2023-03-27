#if not defined(BC69D3A64928461D86423934C1013A50)
#define BC69D3A64928461D86423934C1013A50
#if defined(BC69D3A64928461D86423934C1013A50)

#include "generated.h"
#include "bitfield.h"

#include <algorithm>
#include <array>
#include <bit>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <tuple>
#include <type_traits>
#include <utility>

namespace common_platform {
	namespace detail {
		template <typename F, int M, int E>
		concept common_platform_float = requires
		{
			requires std::floating_point<F>;
			requires sizeof(F)* std::numeric_limits<unsigned char>::digits == M + E;
			requires (std::endian::native == std::endian::little)
			or sizeof(F) == 1;
			requires std::numeric_limits<F>::is_iec559;
			requires std::numeric_limits<F>::digits == M;
			requires std::numeric_limits<F>::radix == 2;
			requires std::numeric_limits<F>::max_exponent == (((1ULL << E) - 1) - (((1ULL << E) - 1) / 2));
			requires std::numeric_limits<F>::min_exponent == (2 - (((1ULL << E) - 1) / 2));
			requires alignof(F) <= sizeof(F);
		};

		template <typename I, int N>
		concept common_platform_integral = requires
		{
			requires std::integral<I>;
			requires (std::endian::native == std::endian::little)
			or sizeof(I) == 1;
			requires std::has_unique_object_representations_v<I>;
			requires sizeof(I) * std::numeric_limits<unsigned char>::digits == N;
			requires std::numeric_limits<I>::is_integer;
			requires (std::numeric_limits<I>::digits == N and std::unsigned_integral<I>)
			or (std::numeric_limits<I>::digits + 1 == N and std::signed_integral<I>);
			requires alignof(I) <= sizeof(I);
		};

		template <typename T>
		concept common_platform_arithmetic = requires
		{
			requires common_platform_float<T, 24, 8>
			or common_platform_float<T, 53, 11>
			or common_platform_integral<T, 8>
			or common_platform_integral<T, 16>
			or common_platform_integral<T, 32>
			or common_platform_integral<T, 64>;
		};

		template <typename T>
		concept tuple_like_binding = requires { std::tuple_size<T>::value; };

		template <typename T>
		concept trivial_and_standard_layout = std::conjunction_v<std::is_trivial<T>, std::is_standard_layout<T>>;

		template <typename T>
		concept reflectable = requires
		{
			requires not tuple_like_binding<T>;
			requires trivial_and_standard_layout<T>;
			requires std::semiregular<T>;
		};

		template <typename T>
		concept non_empty_aggregate_class = std::conjunction_v<std::is_class<T>, std::negation<std::is_empty<T>>, std::is_aggregate<T>>;

		template <typename T>
		concept reflectable_class = requires
		{
			requires reflectable<T>;
			requires non_empty_aggregate_class<T>;
			requires bool(field_count<T>());
		};

		template <typename... Ts>
		struct is_bitfield final : std::false_type {};
		template <typename... Ts>
		struct is_bitfield<bitfield<Ts...>> final : std::true_type {};
		template <typename... Ts>
		struct is_bitfield<const bitfield<Ts...>> final : std::true_type {};
		template <typename... Ts>
		struct is_bitfield<volatile bitfield<Ts...>> final : std::true_type {};
		template <typename... Ts>
		struct is_bitfield<const volatile bitfield<Ts...>> final : std::true_type {};

		template <typename T>
		concept reflectable_bitfield = requires
		{
			requires non_empty_aggregate_class<T>;
			requires trivial_and_standard_layout<T>;
			requires std::regular<T>;
			requires is_bitfield<T>::value;
			requires bool(std::tuple_size_v<T>);
		};

		template <typename T>
		concept reflectable_scalar = requires
		{
			requires reflectable<T>;
			requires common_platform_arithmetic<T>;
		};

		template <typename T>
		concept reflectable_enum = requires
		{
			requires std::is_enum_v<T>;
			requires reflectable<T>;
			requires common_platform_arithmetic<std::underlying_type_t<T>>;
		};

		template <typename T>
		concept bounded_array = std::is_bounded_array_v<T>;
	}

	template <typename V>
	struct is_transparently_serializable_type final { constexpr auto operator()(std::same_as<bool> auto& result, std::same_as<std::size_t> auto&, std::same_as<std::size_t> auto&) noexcept
	{
		result = false;
	}};

	namespace detail {
		template <reflectable_class C, std::size_t N>
		constexpr auto is_transparently_serializable_member(std::same_as<bool> auto& result, std::same_as<std::size_t> auto& offset, std::same_as<std::size_t> auto& align) noexcept
		{
			using tuple_type = decltype(make_tuple<C, std::integral_constant<std::size_t, field_count<C>()>>{}(std::declval<C&>()));
			using type = std::remove_reference_t<std::tuple_element_t<N, tuple_type>>;
			is_transparently_serializable_type<type>{}(result, offset, align);
		}

		template <reflectable_class C, std::size_t... Is>
		constexpr auto is_transparently_serializable_member(const std::index_sequence<Is...>&, std::same_as<bool> auto& result, std::same_as<std::size_t> auto& offset, std::same_as<std::size_t> auto& align) noexcept
		{
			(is_transparently_serializable_member<C, Is>(result, offset, align), ...);
		}
	}

	template <detail::reflectable_scalar S>
	struct is_transparently_serializable_type<S> final { constexpr auto operator()(std::same_as<bool> auto& result, std::same_as<std::size_t> auto& offset, std::same_as<std::size_t> auto& align) noexcept
	{
		if constexpr (sizeof(S) % alignof(S)) result = false;
		if (offset % sizeof(S)) result = false;
		offset += sizeof(S);
		//alignof(S) may be weaker than sizeof(S), 
		//but we're checking this is laid out for all platforms that support reflectable scalar e.g. Win32 & System V
		align = std::max(sizeof(S), align); 
	}};

	template <detail::reflectable_enum E>
	struct is_transparently_serializable_type<E> final { constexpr auto operator()(std::same_as<bool> auto& result, std::same_as<std::size_t> auto& offset, std::same_as<std::size_t> auto& align) noexcept
	{
		if constexpr (sizeof(E) % alignof(E)) result = false;
		is_transparently_serializable_type<std::underlying_type_t<E>>{}(result, offset, align);
	}};

	template <typename A>
	requires std::is_bounded_array_v<A>
	struct is_transparently_serializable_type<A> final { constexpr auto operator()(std::same_as<bool> auto& result, std::same_as<std::size_t> auto& offset, std::same_as<std::size_t> auto& align) noexcept
	{
		auto arr_off = std::size_t{};
		auto arr_align = std::size_t{ 1 };
		using E = std::remove_all_extents_t<A>;
		is_transparently_serializable_type<E>{}(result, arr_off, arr_align);
		if (arr_off not_eq sizeof(E)) result = false;
		if (arr_off % arr_align) result = false;
		if (offset % arr_align) result = false;
		if (offset % alignof(A)) result = false;
		offset += sizeof(A);
		//Similar to the scalar case, align and arr_align are the portable alignment requirements not the
		//alignment on this platform. alignof(A) is only larger than this if this is manually over-aligned.
		//That's fine, as long as it's a constant e.g. 8 not a variable amount e.g. alignof(long).
		align = std::max({ align, arr_align, alignof(A) });
	}};

	template <detail::reflectable_class C>
	struct is_transparently_serializable_type<C> final { constexpr auto operator()(std::same_as<bool> auto& result, std::same_as<std::size_t> auto& offset, std::same_as<std::size_t> auto& align) noexcept
	{
		auto class_off = std::size_t{};
		auto class_align = std::size_t{ 1 };
		detail::is_transparently_serializable_member<C>(std::make_index_sequence<detail::field_count<C>()>(), result, class_off, class_align);
		if (class_off not_eq sizeof(C)) result = false;
		if (class_off % class_align) result = false;
		if (offset % class_align) result = false;
		if (offset % alignof(C)) result = false;
		offset += sizeof(C);
		align = std::max({ align, class_align, alignof(C) });
	}};

	template <detail::reflectable_bitfield B>
	struct is_transparently_serializable_type<B> final { constexpr auto operator()(std::same_as<bool> auto& result, std::same_as<std::size_t> auto& offset, std::same_as<std::size_t> auto& align) noexcept
	{
		if constexpr (not std::has_unique_object_representations_v<B>) result = false;
		if (offset % alignof(B)) result = false;
		offset += sizeof(B);
		align = std::max(align, alignof(B));
	}};

	namespace detail {
		template <typename T>
		[[nodiscard]] consteval auto is_transparently_serializable_element() noexcept
		{
			auto result = true;
			auto offset = std::size_t{};
			auto align = std::size_t{ 1 };
			is_transparently_serializable_type<std::remove_cvref_t<T>>{}(result, offset, align);
			return result and offset == sizeof(T) and not (offset % align) ? align : std::size_t{};
		}

		template <typename T>
		[[nodiscard]] consteval auto align_or_integral_size() noexcept
		{
			auto result = true;
			auto offset = std::size_t{};
			auto align = std::size_t{ 1 };
			is_transparently_serializable_type<std::remove_cvref_t<T>>{}(result, offset, align);
			return align;
		}


		//This method either tracks offset within a pack of types as a std::size_t or
		//it tracks the trailing zeroes in an object's size as an int. Objects can be
		//at in or following a variable length array can be no stricter aligned than
		//the size of smallest element of a variable length array or item in the pack.
		template <std::size_t N, typename Tuple>
		consteval auto transparently_serializable_pack(std::same_as<bool> auto& result, std::size_t& offset, int& align, bool& holds_int) noexcept
		{
			using E = std::tuple_element_t<N, Tuple>;
			using T = std::conditional_t<std::is_unbounded_array_v<E>, std::remove_extent_t<E>, E>;

			if constexpr (constexpr auto element_alignment = is_transparently_serializable_element<T>()) {
				if (holds_int) {
					if (std::countr_zero(element_alignment) > align)
						result = false;
					align = std::min(align, std::countr_zero(sizeof(T)));
				} else {
					if (std::countr_zero(element_alignment) > std::countr_zero(offset))
						result = false;
					if (std::is_unbounded_array_v<E>) {
						align = std::min(std::countr_zero(offset), std::countr_zero(sizeof(T)));
						holds_int = true;
					} else
						offset += sizeof(T);
				}
			} else
				result = false;
		}

		template <typename Tuple, std::size_t... Is>
		[[nodiscard]] consteval auto transparently_serializable_pack(const std::index_sequence<Is...>&) noexcept
		{
			auto result = true;
			auto holds_int = false;
			auto offset = std::size_t{};
			auto align = 0;
			(transparently_serializable_pack<Is, Tuple>(result, offset, align, holds_int), ...);
			return result;
		}
	}

	template <typename... Ts>
	struct is_transparently_serializable final : std::bool_constant<detail::transparently_serializable_pack<std::tuple<Ts...>>(std::index_sequence_for<Ts...>())> {};

	template <typename... Ts>
	inline constexpr auto is_transparently_serializable_v = is_transparently_serializable<Ts...>::value;

	template <typename... Ts>
	concept transparently_serializable = is_transparently_serializable_v<Ts...>;

	namespace detail {
		template <typename Tuple, std::size_t... Is>
		[[nodiscard]] consteval auto common_alignment_pack(const std::index_sequence<Is...>&) noexcept
		{
			return std::max({ detail::align_or_integral_size<
				std::conditional_t<std::is_unbounded_array_v<std::tuple_element_t<Is, Tuple>>,
				std::remove_extent_t<std::tuple_element_t<Is, Tuple>>,
				std::tuple_element_t<Is, Tuple>>>()... });
		}
	}

	template <typename... Ts>
	struct common_platform_alignment final : std::integral_constant<std::size_t, detail::common_alignment_pack<std::tuple<Ts...>>(std::index_sequence_for<Ts...>())> {};

	template <typename... Ts>
	inline constexpr auto common_platform_alignment_v = common_platform_alignment<Ts...>::value;

	[[maybe_unused]] inline constexpr auto is_common_platform = requires
	{
		requires detail::common_platform_integral<std::uint_least8_t, 8>;
		requires detail::common_platform_integral<std::uint_least16_t, 16>;
		requires detail::common_platform_integral<std::uint_least32_t, 32>;
		requires detail::common_platform_integral<std::uint_least64_t, 64>;
		requires detail::common_platform_integral<std::int_least8_t, 8>;
		requires detail::common_platform_integral<std::int_least16_t, 16>;
		requires detail::common_platform_integral<std::int_least32_t, 32>;
		requires detail::common_platform_integral<std::int_least64_t, 64>;
		requires detail::common_platform_float<float, 24, 8>;
		requires detail::common_platform_float<double, 53, 11>;
	};
}
#endif
#endif
