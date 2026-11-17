#pragma once

namespace Platform
{
    inline HMODULE LoadLibrary(const char* Name)
    {
        char Path[256] = {};
        strcpy(Path, Name);
        strcat(Path, ".dll");

        return LoadLibraryA(Path);
    }

    inline void* GetAddress(HMODULE Library, const char* Function)
    {
        return GetProcAddress(Library, Function);
    }

    inline void FreeLibrary(HMODULE Library)
    {
        ::FreeLibrary(Library);
    }
}

namespace Platform
{
    // Sizes of free, reserved and committed regions of the process address space
    inline void GetVirtualMemoryInfo(size_t* Free, size_t* Reserved, size_t* Committed)
    {
        MEMORY_BASIC_INFORMATION MemoryInfo = {};
        *Free = *Reserved = *Committed = 0;
        while (VirtualQuery(MemoryInfo.BaseAddress, &MemoryInfo, sizeof(MemoryInfo)))
        {
            switch (MemoryInfo.State)
            {
            case MEM_FREE:
                *Free += MemoryInfo.RegionSize;
                break;
            case MEM_RESERVE:
                *Reserved += MemoryInfo.RegionSize;
                break;
            case MEM_COMMIT:
                *Committed += MemoryInfo.RegionSize;
                break;
            }
            MemoryInfo.BaseAddress = (char*)MemoryInfo.BaseAddress + MemoryInfo.RegionSize;
        }
    }
}
