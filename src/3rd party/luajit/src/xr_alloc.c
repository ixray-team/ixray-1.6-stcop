#include <stddef.h>
#include "xr_alloc.h"
#include "lj_def.h"
#include "lj_arch.h"

#if LJ_64 && (LUAJIT_OS == LUAJIT_OS_WINDOWS)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

typedef long (*PNTAVM)(HANDLE handle, void** addr, ULONG zbits,
    size_t* size, ULONG alloctype, ULONG prot);
extern PNTAVM ntavm;

#define NTAVM_ZEROBITS 1
#define MAX_SIZE_T (~(size_t)0)
#define MFAIL ((void *)(MAX_SIZE_T))

#define CHUNK_SIZE (128 * 1024)
#define CHUNK_COUNT 2048
#define MAX_POOLS 8

static int inited = 0;
static int g_poolCount = 0;
void* g_heaps[MAX_POOLS] = { NULL };
char g_heapMaps[MAX_POOLS][CHUNK_COUNT + 1];
char* g_firstFreeChunks[MAX_POOLS];

char* find_free(int size, int* poolIndex);
void* XR_ALLOC_POOL();

void XR_INIT()
{
    if (inited)
        return;
    g_poolCount = 0;
    if (XR_ALLOC_POOL() == NULL)
        return;
    inited = 1;
}

void* XR_ALLOC_POOL()
{
    if (g_poolCount >= MAX_POOLS)
        return NULL;

    void* newHeap = NULL;
    size_t size = CHUNK_SIZE * CHUNK_COUNT;
    long st = ntavm(INVALID_HANDLE_VALUE, &newHeap, NTAVM_ZEROBITS, &size,
        MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);

    if (st != 0)
        return NULL;

    g_heaps[g_poolCount] = newHeap;
    memset(g_heapMaps[g_poolCount], 'x', CHUNK_COUNT);
    g_heapMaps[g_poolCount][CHUNK_COUNT] = '\0';
    g_firstFreeChunks[g_poolCount] = g_heapMaps[g_poolCount];
    return g_heaps[g_poolCount++];
}

void* XR_MMAP(size_t size)
{
    int chunks = size / CHUNK_SIZE;
    int poolIndex = -1;
    char* s = find_free(chunks, &poolIndex);

    if (s == NULL)
    {
        if (XR_ALLOC_POOL() == NULL)
            return MFAIL;
        s = find_free(chunks, &poolIndex);
        if (s == NULL)
            return MFAIL;
    }

    void* ptr = (char*)g_heaps[poolIndex] + CHUNK_SIZE * (s - g_heapMaps[poolIndex]);
    for (int i = 0; i < chunks; i++)
        s[i] = 'a' + chunks - 1;

    if (s == g_firstFreeChunks[poolIndex])
        g_firstFreeChunks[poolIndex] = find_free(1, &poolIndex);

    return ptr;
}

void XR_DESTROY(void* ptr, size_t size)
{
    int poolIndex = -1;
    for (int i = 0; i < g_poolCount; i++)
    {
        if (ptr >= g_heaps[i] && ptr < (char*)g_heaps[i] + CHUNK_SIZE * CHUNK_COUNT)
        {
            poolIndex = i;
            break;
        }
    }
    if (poolIndex == -1)
        return;

    char* s = g_heapMaps[poolIndex] + ((char*)ptr - (char*)g_heaps[poolIndex]) / CHUNK_SIZE;
    int count = size / CHUNK_SIZE;
    for (int i = 0; i < count; i++)
        s[i] = 'x';

    if (s < g_firstFreeChunks[poolIndex])
        g_firstFreeChunks[poolIndex] = s;
}

char* find_free(int size, int* poolIndex)
{
    for (int i = 0; i < g_poolCount; i++)
    {
        char* p = g_firstFreeChunks[i];

        if (p == NULL)
            continue;

        int count = 0;
        while (*p != '\0')
        {
            if (*p == 'x')
                count++;
            else
                count = 0;
            p++;
            if (count == size)
            {
                *poolIndex = i;
                return p - count;
            }
        }
    }
    return NULL;
}

#endif
