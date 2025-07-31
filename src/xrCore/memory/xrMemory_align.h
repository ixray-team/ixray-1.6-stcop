#ifndef xrMemory_alignH
#define xrMemory_alignH
#pragma once

u32		 xr_aligned_msize(void *);
void     xr_aligned_free(void *);
void *   xr_aligned_malloc(size_t, size_t);
void *   xr_aligned_offset_malloc(size_t, size_t, size_t);
void *   xr_aligned_realloc(void *, size_t, size_t);
void *   xr_aligned_offset_realloc(void *, size_t, size_t, size_t);

#endif