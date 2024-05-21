#ifndef __GNUC__
#pragma once
#endif
#ifndef __XR_TYPES_H__
#define __XR_TYPES_H__

#if defined(_MSC_VER)

#define NOMINMAX
#include <basetsd.h>
#if _MSC_VER >= 1900
#  include <stdint.h>
#endif

typedef UINT64		uint64_t;
typedef INT64		int64_t;
typedef UINT32		uint32_t;
typedef INT32		int32_t;
typedef UINT16		uint16_t;
typedef INT16		int16_t;
typedef UINT8		uint8_t;
typedef INT8		int8_t;
typedef	UINT_PTR	uintptr_t;
typedef	INT_PTR		intptr_t;

#if _MSC_VER < 1900
const int16_t INT16_MIN = -32768;
const int16_t INT16_MAX = 0x7fff;
const int32_t INT32_MAX = 0x7fffffff;
const uint8_t UINT8_MAX = 0xff;
const uint16_t UINT16_MAX = 0xffff;
const uint32_t UINT32_MAX = 0xffffffff;
const uint64_t UINT64_MAX = 0xffffffffffffffffull;
#endif

#ifdef _WIN64
#if _MSC_VER < 1900
typedef UINT64 uint_fast32_t;
typedef INT64 int_fast32_t;
typedef UINT64 uint_fast16_t;
typedef INT64 int_fast16_t;

const uint_fast32_t UINT_FAST32_MAX = UINT64_MAX;
#endif
#else
typedef _w64 UINT32 uint_fast32_t;
typedef _w64 INT32 int_fast32_t;
typedef _w64 UINT32 uint_fast16_t;
typedef _w64 INT32 int_fast16_t;

#if _MSC_VER < 1900
const uint32_t UINT_FAST32_MAX = UINT32_MAX;
#endif
#endif

#define PRIu32		"I32u"
#define PRIx32		"I32x"
#define PRIuSIZET	"Iu"

#elif defined(__MINGW32__)

#include <stdint.h>
#include <inttypes.h>

#define PRIuSIZET	"Iu"

#else

#warning unexpected build environment
#include <inttypes.h>

#endif

const uint32_t BAD_IDX = UINT32_MAX;

/*
 *	String types
 */
/*
typedef	char string16		[16];
typedef	char string32		[32];
typedef	char string64		[64];
typedef	char string128		[128];
typedef	char string256		[256];
typedef	char string512		[512];
typedef	char string1024		[1024];
typedef	char string2048		[2048];
typedef	char string4096		[4096];
typedef	char string_path	[2*_MAX_PATH];
*/

#include <cstddef>
#include <cassert>

namespace xray_re {

class xr_error {};

template<typename T> inline bool equivalent(T a, T b, T e = T(1e-6))
{
	return (a < b) ? (b - a < e) : (a - b < e);
}

void die(const char* msg, const char* file, unsigned line);
void msg(const char* format, ...);
void dbg(const char* format, ...);

} // end of namespace xray_re

#ifdef NDEBUG
#define xr_assert(expr)		while (!(expr)) { xray_re::die("assertion failed", __FILE__, __LINE__); break; }
#define xr_not_implemented()	xray_re::die("unimplemented code path", __FILE__, __LINE__)
#define xr_not_expected()	xray_re::die("unexpected code path", __FILE__, __LINE__)
#else
#define xr_assert(expr) assert(expr)
#define xr_not_implemented() assert(0)
#define xr_not_expected() assert(0)
#endif

#define xr_dim(x)	sizeof(x)/sizeof((x)[0])

#define TYPEDEF_STD_VECTOR(type) \
	typedef std::vector< type > type##_vec; \
	typedef std::vector< type >::iterator type##_vec_it; \
	typedef std::vector< type >::const_iterator type##_vec_cit;

#define TYPEDEF_STD_VECTOR_PTR(type) \
	typedef std::vector< type* > type##_vec; \
	typedef std::vector< type* >::iterator type##_vec_it; \
	typedef std::vector< type* >::const_iterator type##_vec_cit;

#endif
