#pragma once

#include <glob.h>
#include <fcntl.h>
#include <errno.h>
#include <stdint.h>
#include <utime.h>
#include <pthread.h>
#include <dlfcn.h>

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
#endif

#define __declspec(t)
#define __forceinline inline
#define _cdecl /* __attribute__((__cdecl__)) */
#define __cdecl /* __attribute__((__cdecl__)) */
#define __stdcall /* __attribute__((__stdcall__)) */

#define CALLBACK
#define HWND void*
#define LPARAM void*
#define WPARAM void*
#define LPVOID void*

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
using BYTE = char;
using UINT_PTR = uint64_t;

using DWORD = uint32_t;
using BOOL = int32_t;
using HRESULT = long;
using LRESULT = long;
using HMODULE = void*;

#define SUCCEEDED(hr) (((HRESULT)(hr)) >= 0)
#define FAILED(hr) (((HRESULT)(hr)) < 0)
#define S_OK 0x00000000
#define S_FALSE 0x10000000
#define E_FAIL 0x80004005

#define xr_strerror(errno, buffer, bufferSize) strerror_r(errno, buffer, sizeof(buffer))
#define xr_interface class

inline unsigned long GetLastError()
{
    return 0;
}