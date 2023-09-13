/***************************************************************************************
* Copyright (C) Anton Kovalev (vertver), 2023. All rights reserved.
* GRR - "Games Require Reflection", library for integrating reflection into games
* MIT License
***************************************************************************************/
#ifndef GRR_DETAIL_NAME_PARSER_INCLUDED
#define GRR_DETAIL_NAME_PARSER_INCLUDED

namespace grr::detail
{
	static constexpr const char* platform_name()
	{
		constexpr const char* name = 

#ifdef _WIN32
			"windows-"
#elif __APPLE__ || __MACH__
			"macos-";
#elif __linux__
			"linux-";
#elif __FreeBSD__
			"freebsd-";
#elif __unix || __unix__
			"unix-";
#else
#error Unsupported system
#endif

#if defined(__clang__)
			"clang-"
#elif defined(__GNUC__)
			"gcc-"
#elif defined(_MSC_VER)
			"msvc-"
#else
#error Unsupported compiler
#endif

#if defined(__x86_64__) || defined(_M_X64)
			"x64";
#elif defined(i386) || defined(__i386__) || defined(__i386) || defined(_M_IX86)
			"x86";
#elif defined(__aarch64__) || defined(_M_ARM64)
			"arm64";
#elif defined(__ARM_ARCH__)
			"arm";
#elif defined(mips) || defined(__mips__) || defined(__mips)
			"mips";
#elif defined(__powerpc) || defined(__powerpc__) || defined(__powerpc64__) || defined(__POWERPC__) || defined(__ppc__) || defined(__PPC__) || defined(_ARCH_PPC)
			"ppc";
#elif defined(__PPC64__) || defined(__ppc64__) || defined(_ARCH_PPC64)	
			"ppc64";
#elif defined(__sparc__) || defined(__sparc)
			"sparc";
#else
#error Unsupported platform
#endif

		return name;
	}

	template <std::size_t...Idxs>
	static constexpr auto substring_as_array(std::string_view str, std::index_sequence<Idxs...>)
	{
		return std::array{ str[Idxs]..., '\0' };
	}

	// https://bitwizeshift.github.io/posts/2021/03/09/getting-an-unmangled-type-name-at-compile-time/
	template<typename T>
	static constexpr auto compiler_type_name(int unused /* hack for newer versions of MSVC */)
	{
		(void)(unused);

#if defined(__clang__)
		constexpr grr::string_view prefix = "[T = ";
		constexpr grr::string_view suffix = "]";
		constexpr grr::string_view function = __PRETTY_FUNCTION__;
#elif defined(__GNUC__)
		constexpr grr::string_view prefix = "with T = ";
		constexpr grr::string_view suffix = "]";
		constexpr grr::string_view function = __PRETTY_FUNCTION__;
#elif defined(_MSC_VER)
		constexpr grr::string_view prefix = __FUNCTION__"<";
		constexpr grr::string_view suffix = ">(int)";
		constexpr grr::string_view function = __FUNCSIG__;
#endif
		constexpr std::size_t start = function.find(prefix) + prefix.size();
		constexpr std::size_t end = function.rfind(suffix);

		static_assert(start < end);
		constexpr auto name = function.substr(start, (end - start));

		return name;
	}


}
#endif