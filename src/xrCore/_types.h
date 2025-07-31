#pragma once
#include <cstdint>
#include <limits>

// Type defs
using s8 = std::int8_t;
using u8 = std::uint8_t;

using s16 = std::int16_t;
using u16 = std::uint16_t;

using s32 = std::int32_t;
using u32 = std::uint32_t;

using s64 = std::int64_t;
using u64 = std::uint64_t;

using f32 = float;
using f64 = double;

using pstr = char*;
using pcstr = const char*;

// windoze stuff
#ifndef _WINDOWS_
	typedef	int				BOOL;
	#define TRUE			true
	#define FALSE			false
#endif

// Type limits
#define type_max(T)		(std::numeric_limits<T>::max())
#define type_min(T)		(-std::numeric_limits<T>::max())
#define type_zero(T)	(std::numeric_limits<T>::min())
#define type_epsilon(T)	(std::numeric_limits<T>::epsilon())

#define int_max			type_max(int)
#define int_min			type_min(int)
#define int_zero		type_zero(int)

#define flt_max			type_max(float)
#define flt_min			type_min(float)
//#define FLT_MAX         3.402823466e+38F        /* max value */
//#define FLT_MIN         1.175494351e-38F        /* min positive value */
#define FLT_MAX			flt_max
#define FLT_MIN			flt_min

#define flt_zero		type_zero(float)
#define flt_eps			type_epsilon(float)

#define dbl_max			type_max(double)
#define dbl_min			type_min(double)
#define dbl_zero		type_zero(double)
#define dbl_eps			type_epsilon(double)

typedef	char	string16	[16];
typedef	char	string32	[32];
typedef	char	string64	[64];
typedef	char	string128	[128];
typedef	char	string256	[256];
typedef	char	string512	[512];
typedef	char	string1024	[1024];
typedef	char	string2048	[2048];
typedef	char	string4096	[4096];

typedef	char	string_path	[2*_MAX_PATH];

typedef wchar_t	wstring_path[sizeof(string_path)];
typedef wchar_t wstring4096[sizeof(string4096)];
typedef wchar_t wstring2048[sizeof(string2048)];
typedef wchar_t wstring1024[sizeof(string1024)];
typedef wchar_t wstring512[sizeof(string512)];
typedef wchar_t wstring256[sizeof(string256)];
typedef wchar_t wstring128[sizeof(string128)];
typedef wchar_t wstring64[sizeof(string64)];
typedef wchar_t wstring32[sizeof(string32)];
typedef wchar_t wstring16[sizeof(string16)];

static_assert((sizeof(wstring_path) / sizeof(wchar_t)) == sizeof(string_path), "must be same length!");
static_assert((sizeof(wstring4096) / sizeof(wchar_t)) == sizeof(string4096), "must be same length!");
static_assert((sizeof(wstring2048) / sizeof(wchar_t)) == sizeof(string2048), "must be same length!");
static_assert((sizeof(wstring1024) / sizeof(wchar_t)) == sizeof(string1024), "must be same length!");
static_assert((sizeof(wstring512) / sizeof(wchar_t)) == sizeof(string512), "must be same length!");
static_assert((sizeof(wstring256) / sizeof(wchar_t)) == sizeof(string256), "must be same length!");
static_assert((sizeof(wstring128) / sizeof(wchar_t)) == sizeof(string128), "must be same length!");
static_assert((sizeof(wstring64) / sizeof(wchar_t)) == sizeof(string64), "must be same length!");
static_assert((sizeof(wstring32) / sizeof(wchar_t)) == sizeof(string32), "must be same length!");
static_assert((sizeof(wstring16) / sizeof(wchar_t)) == sizeof(string16), "must be same length!");