#pragma once
#include <charconv>
#include <cstdlib>
#include <cstring>
#include <system_error>
#include <type_traits>

namespace CharConv
{
	// std::from_chars with a floating-point fallback: Apple libc++ (Xcode 16) has no
	// floating-point from_chars, so there it goes through strtod on a NUL-terminated copy.
	template <typename T>
	inline std::from_chars_result FromChars(const char* First, const char* Last, T& Value)
	{
#if defined(IXR_APPLE_SERIES)
		if constexpr (std::is_floating_point_v<T>)
		{
			char Buffer[128];
			const size_t Length = size_t(Last - First);
			if (Length == 0 || Length >= sizeof(Buffer))
			{
				return { First, std::errc::invalid_argument };
			}

			std::memcpy(Buffer, First, Length);
			Buffer[Length] = 0;

			char* End = nullptr;
			const double Parsed = std::strtod(Buffer, &End);
			if (End == Buffer)
			{
				return { First, std::errc::invalid_argument };
			}

			Value = T(Parsed);
			return { First + (End - Buffer), std::errc{} };
		}
		else
#endif
		{
			return std::from_chars(First, Last, Value);
		}
	}
}
