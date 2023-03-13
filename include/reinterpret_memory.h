#if not defined(D6397F19A531494C952ED8D656D82E24)
#define D6397F19A531494C952ED8D656D82E24
#if defined(D6397F19A531494C952ED8D656D82E24)

#include <cstring>
#include <new>
#include <type_traits>

namespace type_conversion {
	template <typename T, typename U>
	requires std::conjunction_v<std::is_trivially_copyable<T>, std::is_trivially_copyable<U>>
	[[nodiscard]] auto reinterpret_memory(U& data) noexcept
	{
		return std::launder(reinterpret_cast<T*>(std::memmove(&data, &data, sizeof(data))));
	}

	template <typename T, typename U, std::size_t N>
	requires std::conjunction_v<std::is_trivially_copyable<T>, std::is_trivially_copyable<U>>
	[[nodiscard]] auto reinterpret_memory(U(&data)[N]) noexcept
	{
		return std::launder(reinterpret_cast<T*>(std::memmove(&data, &data, sizeof(data))));
	}

	template <typename T, typename U>
	requires std::conjunction_v<std::is_trivially_copyable<T>, std::is_trivially_copyable<U>>
	[[nodiscard]] auto reinterpret_memory(U* data, std::size_t size) noexcept
	{
		return std::launder(reinterpret_cast<T*>(std::memmove(data, data, size)));
	}
}

#endif
#endif
