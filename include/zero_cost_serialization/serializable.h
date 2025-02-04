#ifndef BC69D3A64928461D86423934C1013A50
#define BC69D3A64928461D86423934C1013A50
#ifdef BC69D3A64928461D86423934C1013A50

#include "zero_cost_serialization/generated.h"

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

namespace zero_cost_serialization {
	namespace detail {
		template <typename F, int M, int E>
		concept ieee754_interchange_binary = requires
		{
			requires std::floating_point<F>;
			requires sizeof(F)* std::numeric_limits<unsigned char>::digits == M + E;
			requires (std::endian::native == std::endian::little or std::endian::native == std::endian::big)
			or sizeof(F) == 1;
			requires std::numeric_limits<F>::is_iec559;
			requires std::numeric_limits<F>::digits == M;
			requires std::numeric_limits<F>::radix == 2;
			requires std::numeric_limits<F>::max_exponent == (((1ULL << E) - 1) - (((1ULL << E) - 1) / 2));
			requires std::numeric_limits<F>::min_exponent == (2 - (((1ULL << E) - 1) / 2));
			requires alignof(F) <= sizeof(F);
		};

		template <typename I, int N>
		concept twos_complement_interchange_binary = requires
		{
			requires std::integral<I>;
			requires (std::endian::native == std::endian::little or std::endian::native == std::endian::big)
			or sizeof(I) == 1;
			requires std::has_unique_object_representations_v<I>;
			requires sizeof(I) * std::numeric_limits<unsigned char>::digits == N;
			requires std::numeric_limits<I>::is_integer;
			requires (std::numeric_limits<I>::digits == N and std::unsigned_integral<I>)
			or (std::numeric_limits<I>::digits + 1 == N and std::signed_integral<I>);
			requires alignof(I) <= sizeof(I);
		};

		struct zero_cost_serialization_traits
		{
			template <typename T>
			static constexpr auto is_representation_compatible() noexcept
			{
				return requires()
				{
					requires ieee754_interchange_binary<T, 24, 8>
						or ieee754_interchange_binary<T, 53, 11>
						or twos_complement_interchange_binary<T, 8>
						or twos_complement_interchange_binary<T, 16>
						or twos_complement_interchange_binary<T, 32>
						or twos_complement_interchange_binary<T, 64>;
					requires std::endian::native == std::endian::little;
					requires ieee754_interchange_binary<float, 24, 8>;
					requires ieee754_interchange_binary<double, 53, 11>;
					requires twos_complement_interchange_binary<std::uint_least8_t, 8>;
					requires twos_complement_interchange_binary<std::uint_least16_t, 16>;
					requires twos_complement_interchange_binary<std::uint_least32_t, 32>;
					requires twos_complement_interchange_binary<std::uint_least64_t, 64>;
					requires twos_complement_interchange_binary<std::int_least8_t, 8>;
					requires twos_complement_interchange_binary<std::int_least16_t, 16>;
					requires twos_complement_interchange_binary<std::int_least32_t, 32>;
					requires twos_complement_interchange_binary<std::int_least64_t, 64>;
				};
			}

			template <typename T>
			static constexpr auto minimum_alignment() noexcept
			{
				return is_representation_compatible<T>() ? sizeof(T) : alignof(T);
			}
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

		template <typename T>
		concept reflectable_scalar = requires()
		{
			requires reflectable<T>;
			requires std::is_integral_v<T> or std::is_floating_point_v<T>;
		};

		template <typename T>
		concept reflectable_enum = requires
		{
			requires std::is_enum_v<T>;
			requires reflectable<T>;
			requires reflectable_scalar<std::underlying_type_t<T>>;
		};

		template <typename T>
		concept bounded_array = std::is_bounded_array_v<T>;
	}

	template <typename V, typename Traits>
	struct is_serializable_type { constexpr auto operator()(bool& result, std::size_t&, std::size_t&) noexcept
	{
		result = false;
	}};

	namespace detail {
		template <reflectable_class C, typename Traits, std::size_t N>
		constexpr auto is_serializable_member(bool& result, std::size_t& offset, std::size_t& align) noexcept
		{
			using tuple_type = decltype(make_tuple<C, std::integral_constant<std::size_t, field_count<C>()>>{}(std::declval<C&>()));
			using type = std::remove_reference_t<std::tuple_element_t<N, tuple_type>>;
			is_serializable_type<type, Traits>{}(result, offset, align);
		}

		template <reflectable_class C, typename Traits, std::size_t... Is>
		constexpr auto is_serializable_member(const std::index_sequence<Is...>&, bool& result, std::size_t& offset, std::size_t& align) noexcept
		{
			(is_serializable_member<C, Traits, Is>(result, offset, align), ...);
		}
	}

	template <detail::reflectable_scalar S, typename Traits>
	struct is_serializable_type<S, Traits> { constexpr auto operator()(bool& result, std::size_t& offset, std::size_t& align) noexcept
	{
		if constexpr (not Traits:: template is_representation_compatible<S>()) result = false;
		else if constexpr (std::popcount(Traits:: template minimum_alignment<S>()) != 1) result = false;
		else if constexpr (sizeof(S) % alignof(S)) result = false;
		else if constexpr (sizeof(S) % Traits:: template minimum_alignment<S>()) result = false;
		else if (offset % alignof(S)) result = false;
		else if (offset % Traits:: template minimum_alignment<S>()) result = false;
		else {
			offset += sizeof(S);
			align = std::max({align, Traits:: template minimum_alignment<S>(), alignof(S)});
		}
	}};

	template <detail::reflectable_enum E, typename Traits>
	struct is_serializable_type<E, Traits> { constexpr auto operator()(bool& result, std::size_t& offset, std::size_t& align) noexcept
	{
		if constexpr (sizeof(E) % alignof(E)) result = false;
		is_serializable_type<std::underlying_type_t<E>, Traits>{}(result, offset, align);
	}};

	template <typename A, typename Traits>
	requires std::is_bounded_array_v<A>
	struct is_serializable_type<A, Traits> { constexpr auto operator()(bool& result, std::size_t& offset, std::size_t& align) noexcept
	{
		auto arr_off = std::size_t{};
		auto arr_align = std::size_t{ 1 };
		using E = std::remove_all_extents_t<A>;
		is_serializable_type<E, Traits>{}(result, arr_off, arr_align);
		if (arr_off not_eq sizeof(E)) result = false;
		else if (arr_off % arr_align) result = false;
		else if (offset % arr_align) result = false;
		else if (offset % alignof(A)) result = false;
		else {
			offset += sizeof(A);
			align = std::max({ align, arr_align, alignof(A) });
		}
	}};

	template <detail::reflectable_class C, typename Traits>
	struct is_serializable_type<C, Traits> { constexpr auto operator()(bool& result, std::size_t& offset, std::size_t& align) noexcept
	{
		auto class_off = std::size_t{};
		auto class_align = std::size_t{ 1 };
		detail::is_serializable_member<C, Traits>(std::make_index_sequence<detail::field_count<C>()>(), result, class_off, class_align);
		if (class_off not_eq sizeof(C)) result = false;
		else if (class_off % class_align) result = false;
		else if (offset % class_align) result = false;
		else if (offset % alignof(C)) result = false;
		else {
			offset += sizeof(C);
			align = std::max({ align, class_align, alignof(C) });
		}
	}};

	namespace detail {
		template <typename T, typename Traits>
		[[nodiscard]] consteval auto is_serializable_element() noexcept
		{
			auto result = true;
			auto offset = std::size_t{};
			auto align = std::size_t{ 1 };
			is_serializable_type<std::remove_cvref_t<T>, Traits>{}(result, offset, align);
			return result and offset == sizeof(T) and not (offset % align) ? align : std::size_t{};
		}

		template <typename T, typename Traits>
		[[nodiscard]] consteval auto align_or_integral_size() noexcept
		{
			auto result = true;
			auto offset = std::size_t{};
			auto align = std::size_t{ 1 };
			is_serializable_type<std::remove_cvref_t<T>, Traits>{}(result, offset, align);
			return align;
		}


		//This method either tracks offset within a pack of types as a std::size_t or
		//it tracks the trailing zeroes in an object's size as an int. Objects can be
		//at in or following a variable length array can be no stricter aligned than
		//the size of smallest element of a variable length array or item in the pack.
		template <typename Tuple, typename Traits, std::size_t N>
		consteval auto serializable_pack(bool& result, std::size_t& offset, int& align, bool& holds_int) noexcept
		{
			using E = std::tuple_element_t<N, Tuple>;
			using T = std::conditional_t<std::is_unbounded_array_v<E>, std::remove_extent_t<E>, E>;

			if constexpr (constexpr auto element_alignment = is_serializable_element<T, Traits>()) {
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

		template <typename Tuple, typename Traits, std::size_t... Is>
		[[nodiscard]] consteval auto serializable_pack(const std::index_sequence<Is...>&) noexcept
		{
			auto result = true;
			[[maybe_unused]] auto holds_int = false;
			[[maybe_unused]] auto offset = std::size_t{};
			[[maybe_unused]] auto align = 0;
			(serializable_pack<Tuple, Traits, Is>(result, offset, align, holds_int), ...);
			return result;
		}
	}


	template <typename... Ts>
	struct is_serializable : std::bool_constant<detail::serializable_pack<std::tuple<Ts...>, detail::zero_cost_serialization_traits>(std::index_sequence_for<Ts...>())> {};

	template <typename... Ts>
	inline constexpr auto is_serializable_v = is_serializable<Ts...>::value;

	template <typename... Ts>
	concept serializable = is_serializable_v<Ts...>;

	namespace detail {
		template <typename Tuple, typename Traits, std::size_t... Is>
		[[nodiscard]] consteval auto alignment(const std::index_sequence<Is...>&) noexcept
		{
			return std::max({ detail::align_or_integral_size<
				std::conditional_t<std::is_unbounded_array_v<std::tuple_element_t<Is, Tuple>>,
				std::remove_extent_t<std::tuple_element_t<Is, Tuple>>,
				std::tuple_element_t<Is, Tuple>>, Traits>()... });
		}
	}

	template <typename... Ts>
	struct alignment : std::integral_constant<std::size_t, detail::alignment<std::tuple<Ts...>, detail::zero_cost_serialization_traits>(std::index_sequence_for<Ts...>())> {};

	template <typename... Ts>
	inline constexpr auto alignment_v = alignment<Ts...>::value;
}

#endif
#endif
