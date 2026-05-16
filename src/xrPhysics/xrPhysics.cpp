// xrPhysics.cpp : Defines the entry point for the DLL application.
//

#include "StdAfx.h"
#include "xrPhysics.h"

#include "../3rd-party/ode/include/ode/memory.h"

#ifdef _MANAGED
#pragma managed(push, off)
#endif

static	void *	ode_alloc	(size_t size)								{ return xr_malloc(size);			}
static	void *	ode_realloc	(void *ptr, size_t oldsize, size_t newsize)	{ return xr_realloc(ptr,newsize);	}
static	void	ode_free	(void *ptr, size_t size)					{ return xr_free(ptr);				}

#ifdef IXR_WINDOWS
BOOL APIENTRY DllMain(HMODULE hModule, DWORD  ul_reason_for_call, LPVOID lpReserved)
{
   	lpReserved;
	switch (ul_reason_for_call)
	{
		case DLL_PROCESS_ATTACH:

			dSetAllocHandler			(ode_alloc		);
			dSetReallocHandler			(ode_realloc	);
			dSetFreeHandler				(ode_free		);

			break;
		case DLL_PROCESS_DETACH:
			break;
	}
	return true;

}
#else
__attribute__((constructor)) 
static void on_library_load(void)
{
    dSetAllocHandler(ode_alloc);
    dSetReallocHandler(ode_realloc);
    dSetFreeHandler(ode_free);
}
#endif

#ifdef _MANAGED
#pragma managed(pop)
#endif
