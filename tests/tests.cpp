#include "bitfield.h"
#include "reinterpret_memory.h"
#include "strict_alias.h"
#include "transparently_serializable.h"
#include "invoke.h"
#include "apply.h"
#include "interchange_float.h"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <set>
#include <type_traits>
#include <unordered_set>
#include <utility>

using i64 = std::int_least64_t;
template <i64 N>
using I64 = std::integral_constant<i64, N>;

using i32 = std::int_least32_t;
template <i32 N>
using I32 = std::integral_constant<i32, N>;

using i16 = std::int_least16_t;
template <i16 N>
using I16 = std::integral_constant<i16, N>;

using i8 = std::int_least8_t;
template <i8 N>
using I8 = std::integral_constant<i8, N>;

using u64 = std::uint_least64_t;
template <u64 N>
using U64 = std::integral_constant<u64, N>;

using u32 = std::uint_least32_t;
template <u32 N>
using U32 = std::integral_constant<u32, N>;

using u16 = std::uint_least16_t;
template <u16 N>
using U16 = std::integral_constant<u16, N>;

using u8 = std::int_least8_t;
template <u8 N>
using U8 = std::integral_constant<u8, N>;

template<typename... Ts>
using B = common_platform::bitfield<Ts...>;

//Tests only work on common platforms due to assumptions about 8 bit bytes etc in hardcoded constants.
//Classes still work, but this was burdensome to write and untestable on platforms I have access to.
static_assert(common_platform::is_common_platform);

static constexpr auto field1 = common_platform::bitfield<std::integral_constant<signed, 7>>{ { {std::byte{0x7B} } } };
static_assert(field1.get<0>() == -5); //signed fast path rank < int at 0.
[[maybe_unused]] static int x = field1;
static_assert(sizeof(field1) == 1);
static constexpr auto field2 = common_platform::bitfield<std::integral_constant<bool, 1>, std::integral_constant<signed, 7>>{ { { std::byte{0xF6} } } };
static_assert(field2.get<1>() == -5); //signed fast path rank < int not at 0.
static_assert(field2.get<0>() == false); //boolean type.
static_assert(sizeof(field2) == 1);
static constexpr auto field3 = common_platform::bitfield<std::integral_constant<int, 4>, std::integral_constant<signed long long, 62>>{ { { std::byte{0xA0}, std::byte{0xBA}, std::byte{0xCB}, std::byte{0xDC}, std::byte{0xED}, std::byte{0xFE}, std::byte{0xAF}, std::byte{0xAA}, std::byte{0x0A} } } };
static_assert(field3.get<1>() == static_cast<signed long long int>(0xEAAAFFEEDDCCBBAA) << (std::numeric_limits<unsigned long long int>::digits - 62) >> (std::numeric_limits<unsigned long long int>::digits - 62));
static constexpr auto field4 = common_platform::bitfield<std::integral_constant<unsigned, 16>, std::integral_constant<signed, 8>, std::integral_constant<unsigned, 32>>{ { { std::byte{0x11}, std::byte{0x22}, std::byte{0x33}, std::byte{0x44}, std::byte{0x55}, std::byte{0x66}, std::byte{0x77} } } };
static_assert(field4.get<0>() == 0x2211); //multibyte fast path
static_assert(field4.get<1>() == 0x33); //single byte fast path
static_assert(field4.get<2>() == 0x77665544); //unaligned field.
static constexpr auto field5 = common_platform::bitfield<std::integral_constant<int, 4>, std::integral_constant<unsigned long long, 60>>{ { { std::byte{0xA0}, std::byte{0xBA}, std::byte{0xCB}, std::byte{0xDC}, std::byte{0xED}, std::byte{0xFE}, std::byte{0xAF}, std::byte{0xAA} } } };
static_assert(field5.get<1>() == static_cast<unsigned long long int>(0xAAAFFEEDDCCBBAA));
static constexpr auto field6 = common_platform::bitfield<std::integral_constant<int, 2>, std::integral_constant<unsigned long long int, 63>>{ { { std::byte{0xA0}, std::byte{0xBA}, std::byte{0xCB}, std::byte{0xDC}, std::byte{0xED}, std::byte{0xFE}, std::byte{0xAF}, std::byte{0xAA} } } };
static_assert(field6.get<1>() == static_cast<unsigned long long int>(0xAAAFFEEDDCCBBAA0 >> 2));
static constexpr auto field7 = []{ std::remove_const_t<decltype(field1)> f7{}; f7.set<0>(-5); return f7; }();
static_assert(field7 == field1);
static constexpr auto field8 = []{ std::remove_const_t<decltype(field2)> f8{}; f8.set<1>(-5); return f8; }();
static_assert(field8 == field2);
static constexpr auto field9 = [x = field3.get<1>()]{ std::remove_const_t<decltype(field3)> f9{}; f9.set<1>(x); f9.s[8] |= std::byte{0x08};  return f9; }();
static_assert(field9 == field3);
static constexpr auto field10 = common_platform::bitfield<std::integral_constant<signed, 7>>{ { {std::byte{5} } } };
static_assert(field10 > field1);
static constexpr auto field11 = common_platform::bitfield < std::integral_constant<bool, 1>, std::integral_constant < std::byte, std::byte{ 7 } >> { { { std::byte{ 0xF6 } } } };
static_assert(std::to_integer<int>(field11.get<1>()) == (-5 & 0x7F));

static_assert(not common_platform::is_transparently_serializable_v<bool>);
static_assert(common_platform::is_transparently_serializable_v<i32>);
static_assert(common_platform::is_transparently_serializable_v<const i32>);
static_assert(common_platform::is_transparently_serializable_v<volatile i32>);
static_assert(common_platform::is_transparently_serializable_v<const volatile i32>);
using B1 = B<I64<32>, U8<5>, I16<7>>;
static_assert(common_platform::is_transparently_serializable_v<B1>);
struct T1 { B1 a; u16 b; };
static_assert(common_platform::is_transparently_serializable_v<T1>);
static_assert(common_platform::is_transparently_serializable_v<const T1>);
struct T2 { B1 a; i16 b; u8 c; };
static_assert(not common_platform::is_transparently_serializable_v<T2>);
struct T3 { const B1 a; u16 b; };
static_assert(not common_platform::is_transparently_serializable_v<T3>);
struct T4 { u16 a; u8 b, c; alignas(u32) B<I64<64>> d; };
static_assert(common_platform::is_transparently_serializable_v<T4>);
struct T5 { alignas(8) B<I64<64>> a; u16 b; u8 c, d; u32 e; };
static_assert(common_platform::is_transparently_serializable_v<T5>);
struct T6 { alignas(4) struct { u16 a[2]; } a; u32 b; };
static_assert(common_platform::is_transparently_serializable_v<T6>);
struct T7 { struct { alignas(4) u16 a[2]; } a; u32 b; };
static_assert(common_platform::is_transparently_serializable_v<T7>);
static_assert(std::is_same_v<decltype(std::declval<B1&>().get<i64>()), i64>);
static_assert(std::is_void_v<decltype(std::declval<B1&>().set(i16{}))>);

static_assert(common_platform::is_transparently_serializable_v<T5, T6>);
static_assert(common_platform::is_transparently_serializable_v<T5, T6[]>);
static_assert(not common_platform::is_transparently_serializable_v<B1, T5>);
static_assert(common_platform::is_transparently_serializable_v<B1[], B1[]>);
static_assert(not common_platform::is_transparently_serializable_v<B1[], T5>);
static_assert(alignof(B1) < alignof(T1));
static_assert(common_platform::is_transparently_serializable_v<B1[], T1>);

struct Foo {};

static_assert(not type_conversion::detail::strict_alias<int, char>);
static_assert(type_conversion::detail::strict_alias<char, int>);
static_assert(not type_conversion::detail::strict_alias<signed char, int>);
static_assert(type_conversion::detail::strict_alias<unsigned char, int>);
static_assert(type_conversion::detail::strict_alias<std::byte, int>);
static_assert(type_conversion::detail::strict_alias<unsigned, int>);
static_assert(type_conversion::detail::strict_alias<int[5], const int[]>);
static_assert(type_conversion::detail::strict_alias<const int[], int[]>);
static_assert(type_conversion::detail::strict_alias<int[], const int[5]>);
static_assert(type_conversion::detail::strict_alias<const int[5], int[5]>);
static_assert(not type_conversion::detail::strict_alias<unsigned*, int*>);
static_assert(type_conversion::detail::strict_alias<const int*, int*>);
static_assert(type_conversion::detail::strict_alias<const int* volatile*, int** const>);
static_assert(type_conversion::detail::strict_alias<const int(* volatile Foo::* const)[20], int(* const Foo::* volatile)[20]>);
static_assert(type_conversion::detail::strict_alias<int (* const*)(int*), int (* volatile*)(int*)>);
static_assert(not type_conversion::detail::strict_alias<int (Foo::*)() const, int (Foo::*)()>);
static_assert(not type_conversion::detail::strict_alias<int (*)(int*), int (*)(const int*)>);
static_assert(not type_conversion::detail::strict_alias<const int (*)(int*), int (*)(int*)>);
static_assert(type_conversion::detail::strict_alias<int (*)(int* const), int (*)(int*)>);
static_assert(not type_conversion::detail::strict_alias<std::pair<int, int>, std::pair<const int, int>>);

static_assert(std::same_as<decltype(type_conversion::strict_alias_cast<const char*>(std::declval<int*>())), const char*>);
static_assert(std::same_as<decltype(type_conversion::strict_alias_cast<char&>(std::declval<int&>())), char&>);
static_assert(std::same_as<decltype(type_conversion::strict_alias_cast<char&&>(std::declval<int&&>())), char&&>);
static_assert(std::same_as<decltype(type_conversion::strict_alias_cast<const char&&>(std::declval<const int&&>())), const char&&>);
static_assert(std::same_as<decltype(type_conversion::strict_alias_cast<const char&>(std::declval<const int&>())), const char&>);

#include <iostream>
#include <cstring>

template <typename... Ts>
auto test() noexcept
{
	auto fun = [](const char*, std::uint32_t u, float p[], std::size_t n)
	{
		std::cout << u << std::endl;	
		std::for_each_n(p, n, [](auto f) { std::cout << f << std::endl; });
	};
	alignas(Ts...) std::byte buf[sizeof(std::uint32_t) + sizeof(float[2])]{};
	std::memcpy(buf, "hello world", sizeof(buf));
	return data_serialization::invoke<Ts...>(fun, std::forward_as_tuple("hello"), buf);
}

int main()
{
	common_platform::bitfield<std::integral_constant<signed, 7>> f{};
	f = signed(-5);
	std::unordered_set<B1> u;
	u.emplace(B1{});
	u.emplace([] {B1 b{}; b.set<0>(1); return b; }());

	std::set<B1> m;
	m.emplace(B1{});
	m.emplace([] {B1 b{}; b.set<0>(1); return b; }());
	m.emplace([] {B1 b{}; b.set<0>(-1); return b; }());

	assert(m.size() == 3);
	assert(u.size() == 2);
	assert(m.begin()->get<0>() == -1);
	assert(std::next(m.begin(), 2)->get<0>() == 1);
	assert(u.contains([] {B1 b{}; b.set<0>(1); return b; }()));
	assert(u.contains(B1{}));

	test<std::uint32_t, float[]>();

	struct foobar { std::uint32_t a; float b[2]; };
	alignas(foobar) std::byte buf[sizeof(std::uint32_t) + sizeof(float[2])]{};

	auto fun = [](std::uint32_t uu, float(&a)[2]) 
	{
		std::cout << uu << std::endl;
		std::ranges::for_each(a, [](auto ff) { std::cout << ff << std::endl; });
		return 73;
	}; //std::uint32_t and exactly 2 floats.
	std::memcpy(buf, "hello world", sizeof(buf));
	assert(73 == data_serialization::apply<foobar>(fun, buf, sizeof(buf)));

	auto fun2 = [](std::uint32_t uu, float p[], std::size_t n) {
		std::cout << uu << std::endl;
		std::for_each_n(p, n, [](auto ff) { std::cout << ff << std::endl; });	
	}; //std::uint32_t and 0+ floats.
	std::memcpy(buf, "flex array!", sizeof(buf));
	data_serialization::apply<foobar>(fun2, buf, sizeof(buf));
	std::uint32_t ifloat = {};
	float ffloat;

	common_platform::bitfield< common_platform::float_constant<float, 24, 8, std::uint_least32_t>> my_float{};
	static_assert(common_platform::detail::is_float_bitfield_element_v<common_platform::float_constant<float, 24, 8, std::uint_least32_t>>);
	assert(0.0f == float(my_float));

	while (ifloat < 0xFFFFFFFF)
	{
		ffloat = data_serialization::to_binary32(ifloat);
		auto i = std::bit_cast<std::uint32_t>(ffloat);
		assert(((ifloat & 0x7FFF'FFFF) > 0x7F'FFFF) or i == ifloat);
		auto ii = data_serialization::from_binary32(ffloat);
		assert(((ifloat & 0x7FFF'FFFF) > 0x7F'FFFF) or ii == ifloat);
		my_float = ffloat;
		assert(((ifloat & 0x7FFF'FFFF) > 0x7F'FFFF) or data_serialization::from_binary32(ffloat) == data_serialization::from_binary32(float(my_float)));
		++ifloat;
	}
	{
		ffloat = data_serialization::to_binary32(ifloat);
		auto i = std::bit_cast<std::uint32_t>(ffloat);
		assert(((ifloat & 0x7FFF'FFFF) > 0x7F'FFFF) or i == ifloat);
		auto ii = data_serialization::from_binary32(ffloat);
		assert(((ifloat & 0x7FFF'FFFF) > 0x7F'FFFF) or ii == ifloat);
		my_float = ffloat;
		assert(((ifloat & 0x7FFF'FFFF) > 0x7F'FFFF) or data_serialization::from_binary32(ffloat) == data_serialization::from_binary32(float(my_float)));
	}

	return 0;
}
