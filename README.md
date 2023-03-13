# Tools for data serialization
This collection of tools is intended to support serialization idioms primarily for networking applicatons. Such idioms, while common especially in low level or legacy code, are difficult to implement for many users and frequently lead to undefined behavior. Reading and writing simple C structs or arrays of such objects to/from the network is fast but pitfalls such as strict aliasing violations, alignment requirements, padding, bitfields, or strange rules about enumeration types have lead to burdensome code generation solutions often with a significant code size and runtime overhead. While popular solutions do provide methods for serializing higher level objects complete with memory allocation, compression, and cross platform formats, this only attempts to enforce an existing cross platform format -- namely the object representation many developers assume their platforms use anyway. These tools use template metaprogramming to enforce or extend features to write normal looking structures containing fixed width types, arrays, bitfields (using the provided template), and even variable length fields in the style of C flexible array members with no overhead compared to the hand written native structures in C or C++.

# common_platform::transparently_serializable
Most C++ implementations operate in environments with a de-facto standard representation for memory objects. Though the C++ standard allows a lot of variance in the memory representation of arithmetic types, platforms likely to be encountered outside of high performance computing or embedded environments have 8 bit characters, two's complement signed integral representation (required since C++20), and 16, 32, and 64 bit non-extended integral types. They also support IEEE754 binary floating point. In addition, as of writing, it is common for platforms to operate in little endian environments.

This trait determines if a type or set of types will be laid out identically in memory on any such platform. Out of the box, a type supports this if it is trivial and standard layout (formerly, POD, colloquially, a 'c struct'), semi-regular, does not contain bitfields, and is composed of double, float, or std::(u)intN_t, arrays/structs/enums of such types, does not support std::tuple_size, and does not contain padding.

Additionally, a constant is provided to determine if you are on a platform that supports these types. common_platform::is_common_platform is true if your platform supports objects of this layout natively. Note: some objects may still be supported e.g. if you don't have an 8 bit char, but do have other types, those types will still work. NOTE: if your platform supports these types, you may use std::(u)int_leastN_t types safely as they will be their minimum width.

Users may specialize is_transparently_serializable_type in namespace common_platform to extend this functionality to their own types which specialize std::tuple_size, contain bitfields, or otherwise are laid out in a way they know will work correctly but cannot be automatically reflected. This has been done for a user class common_platform::bitfield in the header <include/transparently_serializable.h>. To do so implement the following template specialization:
    `common_platform::is_transparently_serializable_type<T>` specialized for your type which contains a method `constexpr auto operator()(std::same_as<bool> auto& result, std::same_as<std::size_t> auto& offset, std::same_as<std::size_t> auto& align) noexcept` which sets result to false if T cannot be placed at this offset in a structure, assuming the structure is suitably aligned for T, as well as updates offset to include the size of T and updates align to be the max of align and the required alignment for T on any platform (if using the above rules, sizeof -- not alignof -- the largest arithemtic or enumeration type in T, or some more strictly specified alignment). Your structure should not contain padding bits.

Limitations:
1. This trait requires C++20 as it depends on __cpp_aggregate_paren_init >= 201902 (GCC10, MSVC 19.28, Clang 16) to handle detecting structures with array members.
2. This trait requires any class types be able to be bound to a structured binding of type auto& and composed into a tuple of references using std::tie without providing user defined specializations of std::tuple_size. The implementation limits on this are commonly 256 members.
3. This library uses fold expressions to apply methods over many sequences. Clang supports 256 elements in a fold expression as a default. User structures cannot contain more than that many objects less one at any level and be used with this library without passing compiler flags to increase this limit.
4. Some amount of misuse cannot be detected. Use of integral types other than fixed width types is not detectable since these are typedefs. Use of enums with the above types or with no explicitly specified underlying types also not detectable. Fortunately, using these types is not common in a networking domain.
5. bitfields e.g. std::uint32_t : 5 are not supported due to inability to reflect on such types. Bitfields can be emulated in a safer more portable way guarenteed to not have padding by using the common_platform::bitfield<> class. Note this can pack multiple types together into a bitfield just like the builtin feature would -- in addition, this class has no padding bits unlike the builtin feature in many scenarios.

```
struct foo { std::uint_least32_t x; float y; };
struct bar { std::uint_least32_t x; double y; };
static_assert(not common_platform::is_common_platform or common_platform::is_transparently_serializable_v<foo>); //transparently serializable on all platforms supporting this object representation.
static_assert(not common_platform::is_common_platform or not common_platform::is_transparently_serializable_v<bar>); //true on all platforms. this is padded on SOME platforms that support this object representation, even though it's not padded on all of them.

```

Packs of types are supported, which model if a buffer could contain a sequence of several types which are each individually transparently serializable. Packs support unbounded arrays e.g. T[], which are intended to model C style flexible array members. Use cases for this advanced feature involve data formats which might include some fixed size header followed by a variable number of objects. Simply supply multiple type arguments to the common_platform::is_transparently_serializable_v trait.

# common_platform::bitfield
This class implements a tightly packed sequence of variable width integral/enumeration types. It is a requirement that the type be able to represent as many bits as the field it represents so portable programs should, for example, have a field width not exceeding 16 for int or not exceeding 64 for std::uint_least64_t. Fixed width or minimum width integral types are recommended. The types are arranged in little-endian byte order when they stradle bytes, and access is optimized for byte aligned, word aligned, and little-endian platforms. Setting a field takes care to not clobber adjacent unused bits so padding bits at the end of a structure set to zero can safely be sent across the network. In addition, the layout of adjacent bitfields with mixed type widths or that would stradle the underlying type boundaries is consistent across platforms unlike with the native compiler types.

NOTE: As with builtin types like int, memory is not initialized unless you request it. Unlike builtin types, setting a field may inspect the value of the field. Always aggregate initialize this class unless you intend to copy representation over it e.g. from a network buffer.

TIP: You can use the bitfield class to support an under-aligned or a fixed width integral or enumeration type on platforms that do not support them e.g. `common_platform::bitfield<std::integral_constant<std::uint_least64_t, 64>>`. In fact, even if your platform supports none of the common type representations, common_platform::bitfield supports their binary format. Support for floating point values in the bitfield class is not provided at this time due to the complexity of implementing rounding requirements when converting between the common format and other formats.

For native sized types on common platforms bitfield generates identical assembly to the builtin structures using clang, gcc, or msvc. For bitfields that fit into a native size type, bitfield generates identical assembly on clang, gcc, and msvc except for an extra instruction when padding bits would be set to an indeterminate value in the native representation and bitfield intentionally chooses to preserve their value. This allows setting what would be padding bits in bitfield's representation to 0 for serialization. For cases where the pattern of bitfield does not map to a native bitfield representation on the target architecture, the bitfield class attempts to conformantly generate what would be in practice created from a non-compliant type aliasing cast, and if that is not possible falls back to building up an integral value a byte at a time. On common platforms, this should only happen when a bitfield straddles more than 8 bytes which is not a case that can occur when modeling a native bitfield.

```
//1 bit for a bool, followed by 5 bits as an int_least16_t, followed by enough bits to make a multiple of CHAR_BIT.
common_platform::bitfield<std::integral_constant<bool, 1>, std::integral_constant<std::int_least16_t, 5>> x{};
x.set<bool>(true);
x.set<std::int_least16_t>(-3);
assert(x.get<1>() == -3);
```

# type_conversion::strict_alias_cast
This is a simple wrapper around reinterpret cast which determines if the destination reference or pointer type can safely be used to alias the source type. This is not a transitive nor symmetric property and the type of the object must really be the source type for this to be safe. Accessing the representation of an object through this cast is well defined behavior within the bounds of the strict aliasing rule. Use this where you would use reinterpret_cast to inspect the representation of another type.

```
int x = -7;
auto y = *type_conversion::strict_alias_cast<unsigned>(&x); //okay, x may be inspected as an unsigned.
//auto z = *type_conversion::strict_alias_cast<float>(&x); //fails to compile.
//auto f = *reinterpret_cast<float*>(&x); //undefined behavior.
```

# type_conversion::reinterpret_memory
This is a useful tool for type converting an existing buffer into a new type. The source object must be suitably aligned and sized to contain the new type or the behavior is undefined. This is intended to support creating an object in an aligned buffer read from the network. It also supports behavior analgous of a c style type pun through a union. NOTE: the lifetime of the original object ends if it is not an array of unsigned char or std::byte providing storage for the new type. Only refer to the objects using the resulting pointer from this cast unless referring to the original array of char/std::byte (but not its members).

```
struct foo { std::uint_least32_t x; float y; };
std::byte alignas(foo) buf[sizeof(foo)]{};
auto p = type_conversion::reinterpret_memory<foo>(buf); //p points to a foo, constructed at buf, with the memory representation buf had (as opposed to being indeterminate).
```
This requires the source and destination types be trivially copyable.