#pragma once
#include <dlfcn.h>
#include <alloca.h>
#include <malloc/malloc.h>
#include <mach-o/dyld.h>

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
        strcat(Path, ".dylib");

        snprintf(Path, sizeof(Path), "%s.dylib", Name);
        void* module = dlopen(Path, RTLD_NOW | RTLD_GLOBAL);
        if (module == nullptr)
        {
            snprintf(Path, sizeof(Path), "lib%s.dylib", Name);
            module = dlopen(Path, RTLD_NOW | RTLD_GLOBAL);
            if (module == nullptr)
            {
                printf("%s\n", strerror(errno));

                char exec_path[PATH_MAX] = {};
                uint32_t exec_path_size = sizeof(exec_path);
                _NSGetExecutablePath(exec_path, &exec_path_size);

                int start_of_filename = strlen(exec_path);
                while (start_of_filename > 0 && exec_path[start_of_filename] != '/') {
                    exec_path[start_of_filename--] = '\0';
                }
                exec_path[start_of_filename--] = '\0';

                snprintf(Path, sizeof(Path), "%s/%s.dylib", exec_path, Name);
                module = dlopen(Path, RTLD_NOW | RTLD_GLOBAL);
                if (module == nullptr)
                {
                    snprintf(Path, sizeof(Path), "%s/lib%s.dylib", exec_path, Name);
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