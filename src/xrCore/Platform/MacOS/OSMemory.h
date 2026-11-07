#pragma once
#include <dlfcn.h>
#include <alloca.h>
#include <malloc/malloc.h>

#define _msize malloc_size
#define _expand(p, sz) sz <= _msize(p)
#define _alloca alloca

namespace Platform
{
    inline HMODULE LoadLibrary(const char* Name)
    {
		char Path[PATH_MAX] = {};
		snprintf(Path, sizeof(Path), "%s.dylib", Name);
		void* module = dlopen(Path, RTLD_NOW | RTLD_GLOBAL);
        if (module == nullptr)
        {
			snprintf(Path, sizeof(Path), "lib%s.dylib", Name);
			module = dlopen(Path, RTLD_NOW | RTLD_GLOBAL);
            if (module == nullptr)
            {
				printf("%s\n", dlerror());

				const std::filesystem::path BinaryPath = GetBinaryFolderPath();
				snprintf(Path, sizeof(Path), "%s/%s.dylib", BinaryPath.c_str(), Name);
				module = dlopen(Path, RTLD_NOW | RTLD_GLOBAL);
                if (module == nullptr)
                {
					snprintf(Path, sizeof(Path), "%s/lib%s.dylib", BinaryPath.c_str(), Name);
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

