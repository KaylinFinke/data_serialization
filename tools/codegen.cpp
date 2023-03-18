#include <array>
#include <concepts>
#include <cstddef>
#include <cstdio>
#include <type_traits>
#include <utility>

template <std::size_t N>
[[nodiscard]] consteval auto make_braces() noexcept
{
	std::array<char, N * 3 + not N> s{};
	for (std::size_t i = 0; i not_eq s.size(); ++i)
		s[i] = "{},"[i % 3];
	s.back() = {};
	return s;
}

[[nodiscard]] constexpr auto digits_n(std::integral auto t) noexcept
{
	int i{ t < decltype(t){} };
	do { t /= 10; ++i; } while (t);
	return i;
}

template <std::size_t... S>
[[nodiscard]] constexpr auto digits_pack(std::index_sequence<S...>) noexcept
{
	return (digits_n(S) + ...);
}

template <std::size_t N>
[[nodiscard]] constexpr auto digits() noexcept
{
	return N ? digits_pack(std::make_index_sequence<N>()) : std::size_t{};
}

[[nodiscard]] constexpr auto to_chars(std::same_as<char*> auto end, std::integral auto n) noexcept
{
	auto s = end;
	auto neg = n < 0;
	do {
		*--s = "0123456789"[n % 10];
		n /= 10;
	} while (n);
	if (neg) *--s = '-';
	return end;
}

template <std::size_t N>
[[nodiscard]] consteval auto make_binds() noexcept
{
	std::array<char, 2 * N + not N + digits<N>()> s{};
	char* begin = s.data();
	for (auto n = std::size_t{}; n < N; ++n) {
		*begin++ = 'v';
		begin = to_chars(begin + digits_n(n), n);
		*begin++ = ',';
	}
	s.back() = {};
	return s;
}

template <std::size_t N>
inline constexpr auto braces_n = make_braces<N>();

template <std::size_t N>
inline constexpr auto braces = braces_n<N>.data();

int main()
{
	constexpr auto n = 255;

	puts("#if not defined(C17AB45F97AA4A1EAF3129E2BA17DE70)");
	puts("#define C17AB45F97AA4A1EAF3129E2BA17DE70");
	puts("#if defined(C17AB45F97AA4A1EAF3129E2BA17DE70)");
	puts("");
	puts("#include <cstddef>");
	puts("#include <tuple>");
	puts("#include <type_traits>");
	puts("");
	puts("namespace common_platform::detail {");
	puts("\ttemplate <typename, std::size_t, typename = void>\nstruct init_n final : std::false_type {};");
	puts("");
	for (auto i = 0; i not_eq n + 1; ++i)
		printf("\ttemplate <typename T>\nstruct init_n<T, std::size_t{%i}, std::void_t<decltype(T(%.*s))>> final : std::true_type {};\n\n", i, i ? i * 3 - 1 : 0, braces<n>);
	puts("\ttemplate <typename T, typename = void>\n\tstruct make_tuple final { constexpr auto operator()(T&) const noexcept\n\t{\n\t\treturn std::make_tuple();\n\t}};\n");
	for (auto i = 0, j = digits_n(i) + 1; i not_eq n; j += digits_n(i) + 2)
		printf("\ttemplate <typename T>\n\tstruct make_tuple<T, std::integral_constant<std::size_t, std::size_t{%i}>> final { [[maybe_unused, nodiscard]] constexpr auto operator()(T& t) const noexcept \n\t{\n\t\tauto& [%.*s] = t;\n\t\treturn std::tie(%.*s);\n\t}};\n\n",
			++i, j, make_binds<n>().data(), j, make_binds<n>().data());
	puts("\ttemplate <typename T, std::size_t... Is>");
	puts("\t[[nodiscard]] consteval auto field_count(const std::index_sequence<Is...>&) noexcept");
	puts("\t{");
	puts("\t\tauto n = std::size_t{};");
	puts("\t\t((n = init_n<T, Is>::value ? Is : n), ...);");
	puts("\t\treturn n;");
	puts("\t}");
	puts("");
	puts("\ttemplate <typename T>");
	puts("\t[[nodiscard]] consteval auto field_count() noexcept");
	puts("\t{");
	printf("\t\treturn field_count<T>(std::make_index_sequence<%d>());\n", n + 1);
	puts("\t}");
	puts("}");
	puts("");
	puts("#endif");
	puts("#endif");
	return 0;
}
