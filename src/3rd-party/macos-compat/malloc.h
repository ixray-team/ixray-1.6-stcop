// AI: <malloc.h> replacement for macOS. On Apple this is not a system header at all: glibc's
// AI: version (which the Linux build has) does not exist here, while 24 engine files and the
// AI: fetched DirectXMesh library include it. Provides the allocator declarations they expect
// AI: plus the MSVC aligned allocator. Added to the build on macOS only.
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
