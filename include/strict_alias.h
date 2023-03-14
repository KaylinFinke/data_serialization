#if not defined(CD733C0AAD5448EEB4705E5A0BFA0510)
#define CD733C0AAD5448EEB4705E5A0BFA0510
#if defined(CD733C0AAD5448EEB4705E5A0BFA0510)

#include <concepts>
#include <cstddef>
#include <type_traits>
namespace type_conversion {
	namespace detail {
		template<typename T, typename U>
		struct is_cv_similar : std::is_same<T, U> {};

		template<typename T, typename U>
		struct is_similar : is_cv_similar<std::remove_cv_t<T>, std::remove_cv_t<U>> {};

		template <typename T, typename U>
		struct is_cv_similar<T*, U*> : is_similar<T, U> {};

		template <typename T, typename U, typename S>
		struct is_cv_similar<T S::*, U S::*> : is_similar<T, U> {};

		template <typename T, typename U, std::size_t N>
		struct is_cv_similar<T[N], U[N]> : is_similar<T, U> {};

		template <typename T, typename U, std::size_t N>
		struct is_cv_similar<T[N], U[]> : is_similar<T, U> {};

		template <typename T, typename U, std::size_t N>
		struct is_cv_similar<T[], U[N]> : is_similar<T, U> {};

		template <typename T, typename U>
		struct is_cv_similar<T[], U[]> : is_similar<T, U> {};

		template<typename T, typename U>
		constexpr inline bool is_similar_v = is_similar<T, U>::value;

		template <typename T, typename U>
		concept similar_to = is_similar_v<T, U>;

		template <typename T>
		struct corresponding_signed { using type = T; };

		template <std::unsigned_integral T>
		requires std::negation_v<std::is_same<std::remove_cv_t<T>, bool>>
		struct corresponding_signed<T> { using type = std::make_signed_t<T>; };

		template <typename T>
		struct corresponding_unsigned { using type = T; };

		template <std::signed_integral T>
		struct corresponding_unsigned<T> { using type = std::make_unsigned_t<T>; };

		template <typename T>
		using corresponding_unsigned_type = typename corresponding_unsigned<T>::type;

		template <typename T>
		using corresponding_signed_type = typename corresponding_signed<T>::type;

		template <typename T, typename U>
		concept strict_alias = requires
		{
			requires similar_to<T, U>
			or similar_to<corresponding_unsigned_type<T>, U>
			or similar_to<corresponding_signed_type<T>, U>
			or similar_to<unsigned char, T>
			or similar_to<char, T>
			or similar_to<std::byte, T>;
		};
	}

	template <typename T, typename U>
	requires std::is_pointer_v<T> and detail::strict_alias<std::remove_pointer_t<T>, U>
	[[nodiscard]] auto strict_alias_cast(U* u) noexcept
	{
		return reinterpret_cast<T>(u);
	}

	template <typename T, typename U>
	requires std::is_reference_v<T> and detail::strict_alias<std::remove_reference_t<T>, U>
	[[nodiscard]] decltype(auto) strict_alias_cast(U&& u) noexcept
	{
		return reinterpret_cast<T>(u);
	}
}

#endif
#endif
