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
		auto check_size(std::same_as<bool> auto& more, std::size_t& size) noexcept
		{
			if constexpr (std::is_unbounded_array_v<T>)
				more = false;
			else if (more)
				size += sizeof(T);
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

		template <typename... Ts, std::size_t... Is>
		requires common_platform::is_transparently_serializable_v<Ts...>
		and ((not std::is_unbounded_array_v<std::tuple_element_t<Is, std::tuple<Ts...>>> or 1 + Is == sizeof...(Ts)) and ... and true)
		[[nodiscard]] auto unpack_and_invoke(const std::index_sequence<Is...>&, auto&& f, auto&& args, std::byte* data, const std::size_t size)
		{
			if constexpr (sizeof...(Ts)) {
				auto more = true;
				auto rsize = std::size_t{};
				(check_size<Ts>(more, rsize), ...);
				if (rsize <= size) {
					std::tuple<Ts*...> ptrs;
					auto remaining = size;
					((std::get<Is>(ptrs) = unpack_element<Ts>(data, remaining)), ...);
					using A = std::remove_pointer_t<std::tuple_element_t<sizeof...(Ts) - 1, decltype(ptrs)>>;
					if constexpr (std::is_unbounded_array_v<A>) {
						auto n = (size - rsize) / sizeof(std::remove_extent_t<A>);
						std::apply(f, std::tuple_cat(std::forward<std::remove_reference_t<decltype(args)>>(args), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...), std::forward_as_tuple(n)));
					} else
						std::apply(f, std::tuple_cat(std::forward<std::remove_reference_t<decltype(args)>>(args), std::forward_as_tuple((repack_element(std::get<Is>(ptrs)))...)));
				} else
					return false;
			} else
				std::apply(f, std::forward<std::remove_reference_t<decltype(args)>>(args));
			return true;
		}
	}
	template <typename... Ts>
	requires (common_platform::is_transparently_serializable_v<Ts...>
	and common_platform::is_common_platform)
	[[nodiscard]] auto unpack_and_invoke(auto&& f, std::byte* data, std::size_t size)
	{
		return detail::unpack_and_invoke<Ts...>(std::index_sequence_for<Ts...>(), std::forward<std::remove_reference_t<decltype(f)>>(f), std::make_tuple(), data, size);
	}
	template <typename... Ts>
	requires (common_platform::is_transparently_serializable_v<Ts...>
	and common_platform::is_common_platform)
	[[nodiscard]] auto unpack_and_invoke(auto&& f, auto&& args, std::byte* data, std::size_t size)
	{
		return detail::unpack_and_invoke<Ts...>(std::index_sequence_for<Ts...>(), std::forward<std::remove_reference_t<decltype(f)>>(f), std::forward<std::remove_reference_t<decltype(args)>>(args), data, size);
	}

	template <typename... Ts, std::size_t N>
	requires (common_platform::is_transparently_serializable_v<Ts...>
	and common_platform::is_common_platform)
	[[nodiscard]] auto unpack_and_invoke(auto&& f, std::byte(&data)[N])
	{
		return detail::unpack_and_invoke<Ts...>(std::index_sequence_for<Ts...>(), std::forward<std::remove_reference_t<decltype(f)>>(f), std::make_tuple(), data, sizeof(data));
	}

	template <typename... Ts, std::size_t N>
	requires (common_platform::is_transparently_serializable_v<Ts...>
	and common_platform::is_common_platform)
	[[nodiscard]] auto unpack_and_invoke(auto&& f, auto&& args, std::byte(&data)[N])
	{
		return detail::unpack_and_invoke<Ts...>(std::index_sequence_for<Ts...>(), std::forward<std::remove_reference_t<decltype(f)>>(f), std::forward<std::remove_reference_t<decltype(args)>>(args), data, sizeof(data));
	}
}
#endif
#endif
