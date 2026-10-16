#pragma once
#include <dlfcn.h>
#include <alloca.h>
#include <malloc/malloc.h>

// glibc names this malloc_usable_size; macOS calls it malloc_size.
// The rest of the engine (xrMemory_align.cpp) calls it through the _msize macro below,
// so the equivalent is provided here, in the platform layer.
inline size_t malloc_usable_size(void* p)
{
    return malloc_size(p);
}

#define _msize malloc_usable_size
#define _expand(p, sz) sz <= _msize(p)
#define _alloca alloca

namespace Platform
{
    inline HMODULE LoadLibrary(const char* Name)
    {
        char Path[256] = {};
        strcpy(Path, Name);
        strcat(Path, ".so");

        snprintf(Path, sizeof(Path), "%s.so", Name);
        void* module = dlopen(Path, RTLD_NOW | RTLD_GLOBAL);
        if (module == nullptr)
        {
            snprintf(Path, sizeof(Path), "lib%s.so", Name);
            module = dlopen(Path, RTLD_NOW | RTLD_GLOBAL);
            if (module == nullptr)
            {
                printf("%s\n", strerror(errno));

                char exec_path[PATH_MAX] = {};
                readlink("/proc/self/exe", exec_path, sizeof(exec_path));

                int start_of_filename = strlen(exec_path);
                while (start_of_filename > 0 && exec_path[start_of_filename] != '/') {
                    exec_path[start_of_filename--] = '\0';
                }
                exec_path[start_of_filename--] = '\0';

                snprintf(Path, sizeof(Path), "%s/%s.so", exec_path, Name);
                module = dlopen(Path, RTLD_NOW | RTLD_GLOBAL);
                if (module == nullptr)
                {
                    snprintf(Path, sizeof(Path), "%s/lib%s.so", exec_path, Name);
                    module = dlopen(Path, RTLD_NOW | RTLD_GLOBAL);
                }
            }
        }

        return module;
    }

    inline void* GetAddress(HMODULE Library, const char* Function)
    {
        return dlsym(Library, Function);
    }

    inline void FreeLibrary(HMODULE Library)
    {
        dlclose(Library);
    }
}