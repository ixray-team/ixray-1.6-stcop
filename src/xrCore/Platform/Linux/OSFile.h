#pragma once
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <stdint.h>

#define _wopen open
#define _wfdopen _fdopen

using errno_t = int;

using __int64 = int64_t;
using __time64_t = __int64;
using __time32_t = long;
using _fsize_t =  unsigned long;

inline errno_t fopen_s(FILE **f, const char *name, const char *mode) 
{
    errno_t ret = 0;
    //assert(f);
    *f = fopen(name, mode);
    /* Can't be sure about 1-to-1 mapping of errno and MS' errno_t */
    if (!*f)
        ret = errno;
    return ret;
}

#define _A_HIDDEN 0x02
#define _A_SUBDIR 0x00000010

struct _finddata_t
{
    unsigned attrib;
    __time64_t time_create;
    __time64_t time_access;
    __time64_t time_write;
    _fsize_t size;
    char name[FILENAME_MAX];
};

#if 0
struct _finddata32_t
{
    unsigned attrib;
    __time32_t time_create;
    __time32_t time_access;
    __time32_t time_write;
    _fsize_t size;
    char name[FILENAME_MAX];
};
#endif

class FS_Path;
namespace Platform
{
    IC bool OS_OpenFileWnd(char* buffer, size_t sz_buf, FS_Path* P, int start_flt_ext, char flt[1024], LPCSTR offset, bool bMulti)
    {
        return true
    }
}