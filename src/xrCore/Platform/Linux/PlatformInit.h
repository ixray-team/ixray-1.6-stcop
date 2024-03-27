#pragma once

#define __declspec(t)
#define __forceinline __attribute__((always_inline))
#define _cdecl /* __attribute__((__cdecl__)) */
#define __stdcall /* __attribute__((__stdcall__)) */

#include <inttypes.h>
#include <unistd.h>
#include <float.h>

#define __int64 int64_t
#define _copysign copysign

using xr_special_char = char;
using LPSTR = char*;
using LPCSTR = const char*;
using BYTE = char;