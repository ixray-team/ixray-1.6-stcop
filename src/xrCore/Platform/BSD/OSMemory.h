#pragma once

#define _msize malloc_usable_size
#define _expand(p, sz) sz <= _msize(p)
#define _alloca alloca

namespace Platform
{
    // Not implemented on this platform
    inline void GetVirtualMemoryInfo(size_t* Free, size_t* Reserved, size_t* Committed)
    {
        *Free = *Reserved = *Committed = 0;
    }
}
