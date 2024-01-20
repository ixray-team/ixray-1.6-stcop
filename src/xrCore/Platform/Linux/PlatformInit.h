#pragma once

#include <glob.h>
#include <fcntl.h>
#include <errno.h>
#include <stdint.h>
#include <utime.h>
#include <pthread.h>

#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/types.h>

#include <linux/limits.h>

#define __declspec(t)
#define __forceinline __attribute__((always_inline))
#define _cdecl /* __attribute__((__cdecl__)) */
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

#define xr_strerror(errno, buffer, bufferSize) strerror_r(errno, buffer, sizeof(buffer))

inline unsigned long GetLastError()
{
    return 0;
}