// macOS does not ship <malloc.h> (it is an MSVC/glibc header), and it has no
// _aligned_malloc / _aligned_free (MSVC-specific allocators).
//
// DirectXMesh is a fetched third-party library that we do not modify: it includes
// <malloc.h> and uses the MSVC aligned allocator on non-Windows platforms. This
// compatibility header supplies both, and is added to that target's include path
// on macOS only, so nothing else in the engine sees it.
#pragma once

#include <malloc/malloc.h>
#include <stdlib.h>

static inline void* _aligned_malloc(size_t size, size_t alignment)
{
    void* ptr = nullptr;
    if (posix_memalign(&ptr, alignment, size) != 0)
    {
        return nullptr;
    }
    return ptr;
}

static inline void _aligned_free(void* ptr)
{
    free(ptr);
}
