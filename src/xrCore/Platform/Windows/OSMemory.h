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