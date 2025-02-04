#ifndef D6397F19A531494C952ED8D656D82E24
#define D6397F19A531494C952ED8D656D82E24
#ifdef D6397F19A531494C952ED8D656D82E24

#include <cstring>
#include <new>
#include <type_traits>

namespace zero_cost_serialization {
	template <typename T, typename U>
	requires std::conjunction_v<std::is_trivially_copyable<T>, std::is_trivially_copyable<U>>
	[[nodiscard]] auto reinterpret_memory(U& data) noexcept
	{
		return reinterpret_cast<T*>(std::memmove(&data, &data, sizeof(data)));
	}

	template <typename T, typename U, std::size_t N>
	requires std::conjunction_v<std::is_trivially_copyable<T>, std::is_trivially_copyable<U>>
	[[nodiscard]] auto reinterpret_memory(U(&data)[N]) noexcept
	{
		return reinterpret_cast<T*>(std::memmove(&data, &data, sizeof(data)));
	}

	template <typename T, typename U>
	requires std::conjunction_v<std::is_trivially_copyable<T>, std::is_trivially_copyable<U>>
	[[nodiscard]] auto reinterpret_memory(U* data, std::size_t size) noexcept
	{
		return reinterpret_cast<T*>(std::memmove(data, data, size));
	}
}

#endif
#endif
