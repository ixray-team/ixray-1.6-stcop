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
