#pragma once
#include <dlfcn.h>
#include <alloca.h>

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

namespace Platform
{
    // No address-space walk here: the resident set is reported as committed memory
    inline void GetVirtualMemoryInfo(size_t* Free, size_t* Reserved, size_t* Committed)
    {
        *Free = *Reserved = *Committed = 0;

        FILE* Statm = fopen("/proc/self/statm", "r");
        if (Statm == nullptr)
        {
            return;
        }

        unsigned long long TotalPages = 0;
        unsigned long long ResidentPages = 0;
        if (fscanf(Statm, "%llu %llu", &TotalPages, &ResidentPages) == 2)
        {
            const size_t PageSize = (size_t)sysconf(_SC_PAGESIZE);
            *Committed = (size_t)ResidentPages * PageSize;
            *Reserved = (size_t)(TotalPages - ResidentPages) * PageSize;
        }
        fclose(Statm);
    }
}
