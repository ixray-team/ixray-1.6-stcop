#pragma once

#include <glob.h>
#include <fcntl.h>
#include <errno.h>
#include <stdint.h>
#include <utime.h>
#include <pthread.h>
#include <dlfcn.h>
#include <stddef.h>

#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/types.h>

#include <linux/limits.h>
#include <bits/local_lim.h>

#ifdef IXR_ARM64
#	include <arm64_neon.h>
#	include <sse2neon/sse2neon.h>
#else
#	include <xmmintrin.h>
#   include <x86intrin.h>
#endif

#if __has_include(<stacktrace>)
#	include <stacktrace>
#	define USE_CXX_STACKTRACE 1
#elif __has_include(<experimental/stacktrace>)
#	include <experimental/stacktrace>
#	define USE_CXX_STACKTRACE 1
#else
#	define USE_CXX_STACKTRACE 0
#endif

#define __declspec(t)
#define __forceinline inline
#define _cdecl /* __attribute__((__cdecl__)) */
#define __cdecl /* __attribute__((__cdecl__)) */
#define __stdcall /* __attribute__((__stdcall__)) */

#define CALLBACK
#ifndef XR_USE_DXVK_NATIVE
#   define HWND void*
#   define LPARAM void*
#   define WPARAM void*
#   define LPVOID void*
#endif

#define xr_vectorcall

using UINT = unsigned int;

#include <inttypes.h>
#include <unistd.h>
#include <float.h>

#define __int64 int64_t
#define _copysign copysign

#define _utimbuf utimbuf
#define _utime utime

#define TEXT(a) a

using xr_special_char = char;
using LPSTR = char*;
using LPCSTR = const char*;
using BYTE = unsigned char;
using UINT_PTR = uint64_t;

using DWORD = uint32_t;
using BOOL = int32_t;
using HRESULT = int32_t;
using LRESULT = long;
using HMODULE = void*;
using DWORD_PTR = uintptr_t;
using HFILE = int;

#define TRUE  1
#define FALSE 0


#define SUCCEEDED(hr) (((HRESULT)(hr)) >= 0)
#define FAILED(hr) (((HRESULT)(hr)) < 0)
#define S_OK 0x00000000
#define S_FALSE 0x10000000
#define E_FAIL 0x80004005

#define xr_strerror(errno, buffer, bufferSize) strerror_r(errno, buffer, sizeof(buffer))
#define xr_interface class
#define RGB(r,g,b) ((uint32_t)(((uint8_t)(r)) | ((uint16_t)((uint8_t)(g))<<8) | ((uint32_t)((uint8_t)(b))<<16)))

inline unsigned long GetLastError()
{
    return 0;
}


#ifndef _In_
#define _In_
#endif

#ifndef _In_opt_
#define _In_opt_
#endif

#ifndef _Out_
#define _Out_
#endif

#ifndef _Out_opt_
#define _Out_opt_
#endif

#ifndef _Inout_
#define _Inout_
#endif

#ifndef _In_z_
#define _In_z_
#endif

#ifndef _Outptr_
#define _Outptr_
#endif

#ifndef _Outptr_result_maybenull_
#define _Outptr_result_maybenull_
#endif

#ifndef _Outptr_result_nullonfailure_
#define _Outptr_result_nullonfailure_
#endif

#ifndef _Printf_format_string_
#define _Printf_format_string_
#endif

#ifndef __analysis_assume
#define __analysis_assume(x)
#endif

#ifndef _Ret_maybenull_
#define _Ret_maybenull_
#endif

#ifndef _Post_writable_byte_size_
#define _Post_writable_byte_size_(x)
#endif

#ifndef _Analysis_assume_
#define _Analysis_assume_(x)
#endif

#ifndef _Success_
#define _Success_(x)
#endif

#ifndef _Return_type_success_
#define _Return_type_success_(x)
#endif

#ifndef _In_reads_
#define _In_reads_(x)
#endif
