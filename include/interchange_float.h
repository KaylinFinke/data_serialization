#if not defined(F4936E35FA8C4BD8A6390722AEA95FBB)
#define F4936E35FA8C4BD8A6390722AEA95FBB
#if defined(F4936E35FA8C4BD8A6390722AEA95FBB)

#include <bit>
#include <cmath>
#include <concepts>
#include <cstdint>
#include <limits>


namespace data_serialization {
	namespace detail {
		template <typename I, typename F, int M, int E>
		concept native_float = requires
		{
			requires std::endian::native == std::endian::little
			or std::endian::native == std::endian::big;

			requires std::floating_point<F>;
			requires sizeof(F)* std::numeric_limits<unsigned char>::digits == M + E;
			requires std::numeric_limits<F>::is_iec559;
			requires std::numeric_limits<F>::digits == M;
			requires std::numeric_limits<F>::radix == 2;
			requires std::numeric_limits<F>::max_exponent == (((1ULL << E) - 1) - (((1ULL << E) - 1) / 2));
			requires std::numeric_limits<F>::min_exponent == (2 - (((1ULL << E) - 1) / 2));

			requires std::integral<I>;
			requires std::has_unique_object_representations_v<I>;
			requires sizeof(I)* std::numeric_limits<unsigned char>::digits == M + E;
			requires std::numeric_limits<I>::is_integer;
			requires (std::numeric_limits<I>::digits == M + E and std::unsigned_integral<I>)
			or (std::numeric_limits<I>::digits + 1 == M + E and std::signed_integral<I>);
		};

		template <int M, std::integral T>
		constexpr auto round_to_even(T& t, int& e) noexcept
		{
			constexpr auto i_bit = T{ T{1} << (M - 1) };
			constexpr auto m_mask = T{ i_bit - 1 };
			constexpr auto m_max = i_bit | m_mask;

			if (auto exceeds_m = std::bit_width(static_cast<std::make_unsigned_t<T>>(t)) - M; exceeds_m > 0) [[unlikely]] {
				auto odd_bit = T{ 1 } << exceeds_m;
				auto half = odd_bit >> 1;
				auto is_odd = bool(odd_bit & t);
				auto frac = (t & (odd_bit - 1));

				if ((frac > half) or (frac == half and is_odd)) {
					if ((t >> exceeds_m) == m_max) {
						t = i_bit;
						++e;
					}
					else
						t = (t >> exceeds_m) + 1;
				}
				else
					t >>= exceeds_m;
				e += exceeds_m;
			}
		}
	}
	/*Interprets an integral value as a binary floating point representation.
	 converting the value to as close a representation as possible. If the
	 implementation supports IEEE754 and the value does not represent NaN the
	 result is exact. On some obscure platforms that support IEEE and have a
	 compatible layout to allow bit_cast, the 'quietness' of a NaN value may
	 not be preserved. This is merely a theoretical consideration -- signaling
	 NaN values are automatically quieted anyway in many cases. If the implementation
	 does not support the bit_cast fast path the value of the bits encoded in the
	 mantissa are not preserved for NaNs. This is also mandated by the standard for
	 NaNs anyway, so these two limitations while being an implementation detail are
	 not of practical concern.*/
	template <std::floating_point F, int M, int E, std::integral T>
	requires (std::numeric_limits<T>::digits >= (M + E) - std::is_signed_v<T>)
	F to_float(const T& t) noexcept
	{
		if constexpr (detail::native_float<T, F, M, E>)
			return std::bit_cast<F>(t);
		else {
			constexpr auto i_bit = T{ T{1} << (M - 1) };
			constexpr auto q_bit = T{ i_bit >> 1 };
			constexpr auto m_mask = T{ i_bit - 1 };
			constexpr auto e_shift = (M - 1);
			constexpr auto e_mask = T{ T{ (1 << E) - 1 } << e_shift };
			constexpr auto s_mask = T{ T{1} << (M + E - 1) };
			constexpr auto b = ((1 << E) - 1) >> 1;

			auto m = t & m_mask;
			auto e = signed(t & e_mask) >> e_shift;
			auto s = t & s_mask;
			auto v = s ? F{ -1.0 } : F{ 1.0 };

			if (!e)
				return std::copysign(std::ldexp(F(m), 2 - b - M), v);
			else if (e != e_mask)
				return std::copysign(std::ldexp(F(i_bit | m), e - b - M + 1), v);
			else if (!m) {
				if constexpr (std::numeric_limits<F>::has_infinity)
					return std::copysign(std::numeric_limits<F>::infinity(), v);
				else
					return s ? std::numeric_limits<F>::lowest() : std::numeric_limits<F>::max();
			} else if (m & q_bit) {
				if constexpr (std::numeric_limits<F>::has_quiet_NaN)
					return std::copysign(std::numeric_limits<F>::quiet_NaN(), v);
				else if constexpr (std::numeric_limits<F>::has_signaling_NaN)
					return std::copysign(std::numeric_limits<F>::signaling_NaN(), v);
				else if constexpr (std::numeric_limits<F>::has_infinity)
					return std::copysign(std::numeric_limits<F>::infinity(), v);
				else
					return s ? std::numeric_limits<F>::lowest() : std::numeric_limits<F>::max();
			} else {
				if constexpr (std::numeric_limits<F>::has_signaling_NaN)
					return std::copysign(std::numeric_limits<F>::signaling_NaN(), v);
				else if constexpr (std::numeric_limits<F>::has_quiet_NaN)
					return std::copysign(std::numeric_limits<F>::quiet_NaN(), v);
				else if constexpr (std::numeric_limits<F>::has_infinity)
					return std::copysign(std::numeric_limits<F>::infinity(), v);
				else
					return s ? std::numeric_limits<F>::lowest() : std::numeric_limits<F>::max();
			}
		}
	}

	/*returns a T with the low M+E bits representing an IEEE754 binary encoded float
	 even if the platform doesn't support this natively. If the  implementation supports
	 IEEE754 and the value does not represent NaN the result is exact. NaN values are
	 converted to a qNaN for interchange as specified by IEEE754-2008. If the floating
	 point value has a magnitude exceeding the interchange format the value is encoded
	 as +/-INF. If the floating point value cannot be represented exactly in the
	 interchange format, it is rounded to the current environment rounding method. If the
	 result is an integral number that can't be represented in M bits, it's rounded to even.*/
	template <std::integral T, int M, int E, std::floating_point F>
	requires (std::numeric_limits<T>::digits >= (M + E) - std::is_signed_v<T>)
	T from_float(const F& f) noexcept
	{
		if constexpr (detail::native_float<T, F, M, E>)
			return std::bit_cast<T>(f);
		else {
			T t{};
			constexpr auto i_bit = T{ T{1} << (M - 1) };
			constexpr auto q_bit = T{ i_bit >> 1 };
			constexpr auto m_mask = T{ i_bit - 1 };
			constexpr auto e_shift = (M - 1);
			constexpr auto e_mask = T{ T{ (1 << E) - 1 } << e_shift };
			constexpr auto s_mask = T{ T{1} << (M + E - 1) };
			constexpr auto b = ((1 << E) - 1) >> 1;
			constexpr auto e_min = -b - M + 1;
			constexpr auto e_max = b + 1;

			if (std::isfinite(f)) {
				auto e = int{};
				t = T(std::abs(std::rint(std::ldexp(std::frexp(f, &e), M))));
				
				detail::round_to_even<M>(t, e);

				if (e > e_max)
					t = e_mask;
				else if (!t or e < e_min)
					t = 0;
				else if (e > -b + 1) {
					t &= m_mask;
					t |= (e + b) << e_shift;
				} else
					t >>= std::abs(e + b - 2);
			} else
				t = e_mask;
			if (std::isnan(f)) t |= q_bit;
			if (std::signbit(f)) t |= s_mask;
			return static_cast<T>(t);
		}
	}

	template <std::floating_point F>
	auto to_brain16(const std::integral auto& t)
	{
		return to_float<F, 8, 8>(t);
	}
	template <std::floating_point F>
	auto to_binary16(const std::integral auto& t)
	{
		return to_float<F, 11, 5>(t);
	}
	template <std::floating_point F = float>
	auto to_binary32(const std::integral auto& t)
	{
		return to_float<F, 24, 8>(t);
	}
	template <std::floating_point F = double>
	auto to_binary64(const std::integral auto& t)
	{
		return to_float<F, 53, 11>(t);
	}
	template <std::integral T>
	auto from_brain16(const std::floating_point auto& f)
	{
		return from_float<T, 8, 8>(f);
	}
	template <std::integral T>
	auto from_binary16(const std::floating_point auto& f)
	{
		return from_float<T, 11, 5>(f);
	}
	template <std::integral T = std::uint_least32_t>
	auto from_binary32(const std::floating_point auto& f)
	{
		return from_float<T, 24, 8>(f);
	}
	template <std::integral T = std::uint_least64_t>
	auto from_binary64(const std::floating_point auto& f)
	{
		return from_float<T, 53, 11>(f);
	}
}
#endif
#endif
