#include "zero_cost_serialization/apply.h"
#include "zero_cost_serialization/bitfield.h"
#include <cstring>
#include <random>
#include <span>
#include <string>

// Our system only cares that we're operating on a platform with 8 bit characters.
// Networking without this is perfectly possible, but the APIs would be foreign to us.
static_assert(std::numeric_limits<unsigned char>::digits == 8);

//convenience typedefs to shorten examples.
using flag = bool;

using u8 = std::uint_least8_t;
using u16 = std::uint_least16_t;
using u32 = std::uint_least32_t;
using u64 = std::uint_least64_t;

using i8 = std::int_least8_t;
using i16 = std::int_least16_t;
using i32 = std::int_least32_t;
using i64 = std::int_least64_t;

using f32 = float;
using f64 = double;

//define some types that are serializable on any platform with 8 bit chars.
//You can pack more than one field in a zero_cost_serialization::bitfield but this way fields are
//individually addressable on all platforms at the cost of not having the same representation
//on platforms that don't support 8 bit chars. For this simple example, we choose to simply
//compile time assert this is the case.
namespace net {
	using flag_tag = std::true_type;

	using u8_tag = std::integral_constant<::u8, 8>;
	using u16_tag = std::integral_constant<::u16, 16>;
	using u32_tag = std::integral_constant<::u32, 32>;
	using u64_tag = std::integral_constant<::u64, 64>;

	using i8_tag = std::integral_constant<::i8, 8>;
	using i16_tag = std::integral_constant<::i16, 16>;
	using i32_tag = std::integral_constant<::i32, 32>;
	using i64_tag = std::integral_constant<::i64, 64>;

	using f32_tag = zero_cost_serialization::float_constant<::f32, 24, 8, ::u32>;
	using f64_tag = zero_cost_serialization::float_constant<::f64, 53, 11, ::u64>;

	using flag = zero_cost_serialization::bitfield<flag_tag>;

	using u8 = zero_cost_serialization::bitfield<u8_tag>;
	using u16 = zero_cost_serialization::bitfield<u16_tag>;
	using u32 = zero_cost_serialization::bitfield<u32_tag>;
	using u64 = zero_cost_serialization::bitfield<u64_tag>;

	using i8 = zero_cost_serialization::bitfield<i8_tag>;
	using i16 = zero_cost_serialization::bitfield<i16_tag>;
	using i32 = zero_cost_serialization::bitfield<i32_tag>;
	using i64 = zero_cost_serialization::bitfield<i64_tag>;

	using f32 = zero_cost_serialization::bitfield<f32_tag>;
	using f64 = zero_cost_serialization::bitfield<f64_tag>;

	//some headers, depending on if our message is fixed size or variable length.
	//our protocol is either a fixed or variable length header, followed by a fixed size
	//struct, followed by a variable number of fixed size things up to count.
	using header_fixed = u8;
	struct header_variable
	{
		header_fixed id;
		u8 count;
	};
}

//You can use any type that has a 8/16/32/64 bit int or binary IEEE754 32/64 bit float representation 
//directly on your platforms, but be sure to check it's the right size using numeric_limits and account
//for alignment requirements. it's much easier to just use the bitfield class, which is what 
//net:: types are in this example. This is a type we'll use in our example protocol.
static_assert(std::numeric_limits<unsigned char>::digits == 8);
static_assert(zero_cost_serialization::is_serializable_v<u8>);
struct unit_id { u8 guid[16]; auto operator<=>(const unit_id&) const = default; };

//Some enumerations and their portably networkable variants. Take care to set an underlying type.
enum class dragon_color : u16 {
	green,
	blue,
	white,
	black,
	red,
	gold,
	last,
};

constexpr const char* color_name[std::size_t(dragon_color::last)] = { "green", "blue", "white", "black", "red", "gold" };

enum class dragon_result : u32 {
	no_dragon,
	miss,
	wounded,
	killed,
	last,
};

//Networkable versions of these types with minimal alignment requirement. 
//Note how we picked dragon result to be a 2 bit bitfield. It's individually addressable so it takes up 
//an entire byte, but it doesn't take up 4.
namespace net
{
	using dragon_color = zero_cost_serialization::bitfield < std::integral_constant < ::dragon_color, ::dragon_color{ 16 } >> ;
	using dragon_result = zero_cost_serialization::bitfield < std::integral_constant < ::dragon_result, ::dragon_result{ 2 } >> ;
}

//Define server and client runtime representations of a dragon.
namespace server {
	struct dragon
	{
		unit_id id;
		std::string name;
		dragon_color color;
		u8 max_health;
		u8 health;
		bool can_fly;
	};
}

namespace client {
	struct dragon
	{
		unit_id id;
		std::string name;
		dragon_color color;
	};
}

namespace server {
	//Messages the server sends to us.
	struct hatch_dragon
	{
		unit_id id;
		//A net::dragon_color is exactly like a dragon_color but has a 1 byte alignment requirement.
		net::dragon_color color;
		net::flag can_fly;
		char name[16];
	};

	struct attack_result
	{
		unit_id id;
		//A net::dragon_result only takes up 1 byte, since the field width is 2 bits, even though the enum is a u32.
		net::dragon_result result;
	};
}

//define some state objects for our server and client representation. 
//This is mostly a set of network buffers, and the status of the dragon as known to this entity.
namespace server {
	struct user_context {
		bool told_dragon;
		std::size_t in_size, out_size;
		std::byte in[1024];
		std::byte out[1024];
	};

	struct server_context {
		bool has_dragon;
		dragon active_dragon;
		std::size_t user_size;
		user_context users[5];
	};
}

namespace client {
	struct user_context {
		bool has_dragon;
		int id;
		dragon active_dragon;
		std::size_t in_size, out_size;
		std::byte in[1024];
		std::byte out[1024];
	};
}

namespace client {
	//Messages the client sends to the server.
	struct attack_dragon
	{
		unit_id id;
		net::flag arrow_or_sword; //a net::flag aka bool is safe to copy even though bool isn't.
	};
}

//Handlers for the server to handle client messages.
namespace server {
	struct handle_attack_dragon
	{
		using message_type = client::attack_dragon;
		bool operator()(server_context*, user_context*, const unit_id&, bool) noexcept;
	};
}

//Handlers for the client to handle server messages.
namespace client {
	struct handle_hatch_dragon
	{
		using message_type = server::hatch_dragon;
		//changing the last type to a pointer the final member, less one extent, and adding a size_t
		//shows that this handles a variable length message similar to C's flexible array member feature.
		bool operator()(client::user_context*, const unit_id&, dragon_color, bool, char[], std::size_t) noexcept;
	};
	struct handle_attack_result
	{
		using message_type = server::attack_result;
		bool operator()(client::user_context*, const unit_id&, dragon_result) noexcept;
	};
}

//We'll define our message tables, and our handler context here. 
//Note, in this example message ids are inferred from table order and not explicitly part of a message.
namespace client {
	using handlers = std::tuple<
		handle_attack_result,
		handle_hatch_dragon
	>;

	using context = std::tuple<user_context*>;
}

namespace server {
	using handlers = std::tuple<
		handle_attack_dragon
	>;

	using context = std::tuple<server::server_context*, server::user_context* >;
}

//Utility functions specific to our example. These parse messages for our RPC system -- note there's no actual
//parse step for messages themselves they're just in the right format already. This simple RPC system has a
//variable length header of 1-2 bytes, followed by a fixed or variable length message.
namespace {
	template <typename H, typename T, std::size_t N>
	consteval auto message_id(std::size_t& i, bool& unique) noexcept
	{
		if constexpr (std::is_same_v<T, typename std::tuple_element_t<N, H>::message_type>) {
			unique = i == std::tuple_size_v<H>;
			i = N;
		}
	}

	template <typename H, typename T, std::size_t... Is>
	[[nodiscard]] consteval auto message_id(const std::index_sequence<Is...>&) noexcept
	{
		auto unique = true;
		auto i = std::tuple_size_v<H>;
		(message_id<H, T, Is>(i, unique), ...);
		if (not unique) i = std::tuple_size_v<H>;
		return i;
	}

	template <typename H, typename T>
	constexpr auto message_id_v = message_id<H, T>(std::make_index_sequence<std::tuple_size_v<H>>());

	//See if the handler signature can be invoked with a variable number of arguments. does not need to be defined.
	template <typename H, typename T, typename Args = std::tuple<>>
	constexpr auto is_variable_length_message = bool(zero_cost_serialization::flex_element_size_v<T, std::tuple_element_t<message_id_v<H, T>, H>, Args>);

	template <typename H, typename T>
	constexpr auto is_message = message_id_v<H, T> != std::tuple_size_v<H>;

	template <typename H, typename Args, zero_cost_serialization::serializable T>
	requires (is_message<H, T> and not is_variable_length_message<H, T, Args>
	and alignof(net::header_fixed) == 1 and alignof(T) == 1 and zero_cost_serialization::serializable<net::header_fixed, T>)
	auto send_message(std::byte (&data)[1024], std::size_t& size, const T& message) noexcept
	{
		net::header_fixed header{}; //always initialize net types. assignment may read the current value.
		header = message_id_v<H, T>;

		if (sizeof(data) - size < sizeof(header) + sizeof(message)) return false;

		std::memcpy(std::span(data).subspan(size, sizeof(header)).data(), reinterpret_cast<const std::byte*>(&header), sizeof(header));
		std::memcpy(std::span(data).subspan(size + sizeof(header), sizeof(message)).data(), reinterpret_cast<const std::byte*>(&message), sizeof(message));
		size += sizeof(header) + sizeof(message);
		return true;
	}

	template <typename H, typename Args, zero_cost_serialization::serializable T>
	requires (is_message<H, T> and is_variable_length_message<H, T, Args>
	and alignof(net::header_variable) == 1 and alignof(T) == 1 and zero_cost_serialization::serializable<net::header_variable, T>)
	auto send_message(std::byte (&data)[1024], std::size_t& size, const T& message, std::size_t n) noexcept
	{
		net::header_variable header{};
		header.id = message_id_v<H, T>;
		header.count = u8(n);
		using handler = std::tuple_element_t<message_id_v<H, T>, H>;
		auto vsize = zero_cost_serialization::flex_element_size_v<T, handler, Args> * n + zero_cost_serialization::apply_size_v<T, handler, Args>;

		if (sizeof(data) - size < sizeof(header) + vsize) return false;

		std::memcpy(std::span(data).subspan(size, sizeof(header)).data(), reinterpret_cast<const std::byte*>(&header), sizeof(header));
		std::memcpy(std::span(data).subspan(size + sizeof(header), vsize).data(), reinterpret_cast<const std::byte*>(&message), vsize);
		size += vsize + sizeof(header);
		return true;
	}

	template <typename T>
	[[nodiscard]] auto recv_header(const std::span<std::byte>& data)
	{
		if (data.size() < sizeof(T)) return static_cast<T*>(nullptr);
		return zero_cost_serialization::reinterpret_memory<T>(data.first(sizeof(T)).data(), sizeof(T));
	}

	template <typename Hdr, typename T, typename F, typename Args>
	[[nodiscard]] auto recv_message_body(Args&& args, const std::span<std::byte>& data, u8 count = u8())
	{
		//The size needed to call zero_cost_serialization::apply without copying.
		//That is, the memory required to construct the minimal number of arguments to F.
		constexpr auto size = zero_cost_serialization::apply_size_v<T, F, Args>;
		//If F signifies that T should be interpreted as a struct with a flexible array member E at the end,
		//This is sizeof(std::remove_extent_t<E>).
		constexpr auto element_size = zero_cost_serialization::flex_element_size_v<T, F, Args>;
		auto message_size = size + count * element_size;
		if (data.size() < message_size) return std::size_t{};
		//Where as std::apply calls F with a std::tuple, zero_cost_serialization::apply calls F constructing the elements of
		//T in the supplied byte buffer. There is a corresponding zero_cost_serialization::invoke call which takes a parameter
		//pack instead of a struct.
		if (zero_cost_serialization::apply<T>(F{}, std::forward<Args>(args), data.first(message_size)))
			return message_size + sizeof(Hdr);
		return std::size_t{};
	}

	template <typename H, std::size_t ID, typename Args>
	[[nodiscard]] auto recv_message_id(Args&& args, const std::span<std::byte>& data)
	{
		using F = std::tuple_element_t<ID, H>;
		using T = typename F::message_type;
		using Hdr = std::conditional_t<bool(zero_cost_serialization::flex_element_size_v<T, F, Args>), net::header_variable, net::header_fixed>;
		if (auto header = recv_header<Hdr>(data)) {
			if constexpr (std::is_same_v<Hdr, net::header_variable>)
				return recv_message_body<Hdr, T, F>(std::forward<Args>(args), data.subspan(sizeof(Hdr)), header->count);
			else
				return recv_message_body<Hdr, T, F>(std::forward<Args>(args), data.subspan(sizeof(Hdr)));
		} else
			return std::size_t{};
	}

	template <typename H, std::size_t Lo = std::size_t{}, std::size_t Hi = std::tuple_size_v<H> -1, typename Args >
	[[nodiscard]] auto find_handler(u8 id, Args&& args, const std::span<std::byte>& data)
	{
		constexpr auto M = Lo + (Hi - Lo) / 2;
		if (id == M) return recv_message_id<H, M, Args>(std::forward<Args>(args), data);
		else if (id < M) {
			if constexpr (M > Lo) return find_handler<H, Lo, M - 1>(id, std::forward<Args>(args), data);
		} else {
			if constexpr (M < Hi) return find_handler<H, M + 1, Hi>(id, std::forward<Args>(args), data);
		}
		return std::size_t{};
	}

	//This is a simple recursive-descent parser that looks for message = message_id [field_count] message_body,
	//unpacks the message body, and calls the associated handler. More typical implementations might use a switch
	//or table of function pointers instead of a tuple of functors -- this is simply illustrating another way to
	//structure such a system.
	template <typename H, typename Args>
	[[nodiscard]] auto recv_message(Args&& args, const std::span<std::byte>& data)
	{
		if (auto header = recv_header<net::header_fixed>(data); header and u8(*header) < std::tuple_size_v<H>)
			return find_handler<H>(*header, std::forward<Args>(args), data);
		return std::size_t{};
	}
}

//Some convenience functions for the server and client to call send_message with the appropriate
//handler table to deduce message ids and handler signatures.
namespace server {
	template <zero_cost_serialization::serializable T>
	auto send_message(std::byte (&data)[1024], std::size_t& size, const T& message) noexcept
	{
		return ::send_message<client::handlers, client::context>(data, size, message);
	}
	template <zero_cost_serialization::serializable T>
	auto send_message(std::byte (&data)[1024], std::size_t& size, const T& message, std::size_t n) noexcept
	{
		return ::send_message<client::handlers, client::context>(data, size, message, n);
	}
}
namespace client {
	template <zero_cost_serialization::serializable T>
	auto send_message(std::byte (&data)[1024], std::size_t& size, const T& message) noexcept
	{
		return ::send_message<server::handlers, server::context>(data, size, message);
	}
	template <zero_cost_serialization::serializable T>
	auto send_message(std::byte (&data)[1024], std::size_t& size, const T& message, std::size_t n) noexcept
	{
		return ::send_message<server::handlers, server::context>(data, size, message, n);
	}
}

namespace {
	//This is a toy application, so just memcpy random numbers of bytes between client/server objects to simulate networking. It's O(n^2) for sending a whole buffer
	//in the worst case, but real applications can use a bipartite buffer. fake_networking simulates a reliable ordered byte stream with no explicit message boundaries
	//similar to TCP copying a random number of bytes from output buffers on one side to input buffers on the other.
	auto fake_tcp_networking(server::server_context& svr, const std::span<client::user_context, 5>& usrs, std::size_t i)
	{
		std::random_device seed;
		std::default_random_engine engine(seed());
		auto svr_users = std::span(svr.users);
		{
			std::uniform_int_distribution<unsigned short> distribution(0, static_cast<unsigned short>(std::min(usrs[i].out_size, sizeof(svr_users[i].in) - svr_users[i].in_size)));
			auto n = distribution(engine);
			if (n)
				std::memcpy(std::span(svr_users[i].in).subspan(svr_users[i].in_size, n).data(), usrs[i].out, n);
			usrs[i].out_size -= n;
			if (usrs[i].out_size and n)
				std::memmove(usrs[i].out, std::span(usrs[i].out).subspan(n, usrs[i].out_size).data(), usrs[i].out_size);

			svr_users[i].in_size += n;
		}
		{
			std::uniform_int_distribution<unsigned short> distribution(0, static_cast<unsigned short>(std::min(sizeof(usrs[i].in) - usrs[i].in_size, svr_users[i].out_size)));
			auto n = distribution(engine);
			if (n)
				std::memcpy(std::span(usrs[i].in).subspan(usrs[i].in_size, n).data(), svr_users[i].out, n);
			svr_users[i].out_size -= n;
			if (svr_users[i].out_size and n)
				std::memmove(svr_users[i].out, std::span(svr_users[i].out).subspan(n, svr_users[i].out_size).data(), svr_users[i].out_size);

			usrs[i].in_size += n;
		}
	}

	auto try_build_new_dragon(server::server_context& svr)
	{
		if (svr.has_dragon) return;
		std::random_device seed;
		std::default_random_engine engine(seed());
		std::uniform_int_distribution<unsigned> distribution(0, std::numeric_limits<unsigned char>::max());
		for (auto& b : svr.active_dragon.id.guid)
			b = u8(distribution(engine));
		svr.active_dragon.color = dragon_color(std::uniform_int_distribution<int>(0, 5)(engine));
		svr.active_dragon.health = svr.active_dragon.max_health = u8(std::uniform_int_distribution<int>(10, 25)(engine));
		svr.has_dragon = true;
		static constexpr const char* names[7] = { "Susan", "Geoff", "Princess", "Dragonette", "Carl", "Muffin", "Whiskers" };
		svr.active_dragon.name = std::span(names)[std::size_t(std::uniform_int_distribution<int>(0, 6)(engine))];
		svr.active_dragon.can_fly = bool(std::uniform_int_distribution<int>(0, 1)(engine));
		std::for_each_n(svr.users, svr.user_size, [&](auto& usr) { usr.told_dragon = false; });
	}

	auto try_tell_current_dragon(server::server_context& svr, server::user_context& usr)
	{
		if (usr.told_dragon) return;

		server::hatch_dragon message{};
		message.id = svr.active_dragon.id;
		message.color = svr.active_dragon.color;
		message.can_fly = svr.active_dragon.can_fly;
		std::copy_n(svr.active_dragon.name.c_str(), svr.active_dragon.name.size(), message.name);
		usr.told_dragon = server::send_message(usr.out, usr.out_size, message, svr.active_dragon.name.size() + 1);
	}

	auto try_handle_network(server::server_context& svr, server::user_context& usr)
	{
		auto read = std::size_t{};
		for (auto n = std::size_t{}; (n = recv_message<server::handlers>(std::make_tuple(&svr, &usr), std::span(usr.in).first(usr.in_size).subspan(read))); read += n);

		if (read != usr.in_size and read)
			std::memmove(usr.in, std::span(usr.in).first(usr.in_size).subspan(read, usr.in_size - read).data(), usr.in_size - read);
		usr.in_size -= read;
	}

	auto server_loop(server::server_context& svr)
	{
		try_build_new_dragon(svr);

		std::for_each_n(svr.users, svr.user_size, [&](auto& usr) {
			try_handle_network(svr, usr);
			try_tell_current_dragon(svr, usr);
			});
	}

	auto try_handle_network(client::user_context& usr)
	{
		auto read = std::size_t{};
		for (auto n = std::size_t{}; (n = recv_message<client::handlers>(std::make_tuple(&usr), std::span(usr.in).first(usr.in_size).subspan(read))); read += n);

		if (read != usr.in_size and read)
			std::memmove(usr.in, std::span(usr.in).first(usr.in_size).subspan(read, usr.in_size - read).data(), usr.in_size - read);
		usr.in_size -= read;
	}

	auto try_kill_current_dragon(client::user_context& usr)
	{
		if (not usr.has_dragon) return;
		std::random_device seed;
		std::default_random_engine engine(seed());
		client::attack_dragon message{};
		message.id = usr.active_dragon.id;
		message.arrow_or_sword = bool(std::uniform_int_distribution<int>(0, 1)(engine));
		client::send_message(usr.out, usr.out_size, message);
	}

	auto client_loop(client::user_context& usr)
	{
		try_handle_network(usr);

		try_kill_current_dragon(usr);
	}
}


bool server::handle_attack_dragon::operator()(server_context* svr, server::user_context* usr, const unit_id& id, bool arrow_or_sword) noexcept
{
	server::attack_result message{};
	message.id = id;

	if (not svr->has_dragon or svr->active_dragon.id != id) message.result = dragon_result::no_dragon;
	else if (svr->active_dragon.can_fly and not arrow_or_sword) message.result = dragon_result::miss;
	else if (arrow_or_sword and svr->active_dragon.health <= 1) message.result = dragon_result::killed;
	else if (not arrow_or_sword and svr->active_dragon.health <= 2) message.result = dragon_result::killed;
	else message.result = dragon_result::wounded;

	if (server::send_message(usr->out, usr->out_size, message)) {

		if (dragon_result(message.result) == dragon_result::killed) svr->has_dragon = false;
		else if (arrow_or_sword and dragon_result(message.result) == dragon_result::wounded) svr->active_dragon.health -= 1;
		else if (not arrow_or_sword and dragon_result(message.result) == dragon_result::wounded) svr->active_dragon.health -= 2;
		return true;
	}

	return false; //note: for our RPC system, returning false says try again later. In this case we don't have resources to attack the dragon and sync the client as required.
}

bool client::handle_attack_result::operator()(client::user_context* usr, const unit_id& id, dragon_result result) noexcept
{
	//false results indicate a bug in our stack/server. we can't continue.
	if (result > dragon_result::last) std::terminate();
	if ((not usr->has_dragon or usr->active_dragon.id != id) and result != dragon_result::no_dragon) std::terminate();

	switch (result) {
	case dragon_result::no_dragon:
		printf("knight %d swung at a dragon but she was already slain!\n", usr->id);
		break;
	case dragon_result::miss:
		printf("the dragon %s has dodged knight %d's attack!\n", usr->active_dragon.name.c_str(), usr->id);
		break;
	case dragon_result::wounded:
		printf("knight %d has delivered a mighty blow to a dragon named %s!\n", usr->id, usr->active_dragon.name.c_str());
		break;
	case dragon_result::killed:
		usr->has_dragon = false;
		printf("knight %d has killed a dragon named %s!\n", usr->id, usr->active_dragon.name.c_str());
		break;
	case dragon_result::last:
		//server sent us a bad value; don't know how to continue.
		std::terminate();
	}

	return true;
}


bool client::handle_hatch_dragon::operator()(client::user_context* usr, const unit_id& id, dragon_color color, bool, char name[], std::size_t len) noexcept
{
	//false results indicate a bug in our stack/server. we can't continue.
	if (not len or std::span(name, len)[len - 1] or std::memchr(name, 0, len - 1) or len > sizeof(server::hatch_dragon::name)) std::terminate();
	if (color >= dragon_color::last) std::terminate();

	printf("knight %d has spotted a fearsome %s dragon named %s!\n", usr->id, std::span(color_name)[std::size_t(color)], name);

	//note: our knights are dumb and ignore if the dragon can fly.
	usr->has_dragon = true;
	usr->active_dragon.id = id;
	usr->active_dragon.color = color;
	usr->active_dragon.name = name;

	return true;
}


int main()
{
	server::server_context svr{};
	client::user_context usr[5]{};

	//add some clients.
	svr.user_size = 5;
	for (auto i = 0; auto& u : usr)
		u.id = i++;

	for (auto turn = std::size_t{}; turn < 50; ++turn) {
		for (auto i = std::size_t{}; i < std::extent_v<decltype(usr)>; ++i)
			fake_tcp_networking(svr, usr, i);
		for (auto& u : usr)
			client_loop(u);
		server_loop(svr);
	}
}
