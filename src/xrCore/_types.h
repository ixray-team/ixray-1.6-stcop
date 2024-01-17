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