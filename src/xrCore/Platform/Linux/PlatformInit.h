#pragma once

#define __declspec(t)
#define __forceinline __attribute__((always_inline))

#include <inttypes.h>
#include <unistd.h>
#include <float.h>

#define __int64 int64_t
#define _close close
#define _read read
#define _lseek64 lseek64
#define _O_RDONLY O_RDONLY
#define _open open
#define _lseeki64 lseek64
#define _lseek lseek
#define stricmp strcasecmp

using xr_special_char = char;
using LPCSTR = char*;
using LPCSTR = const char*;
