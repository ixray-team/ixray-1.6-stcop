#include "stdafx.h"

#pragma hdrstop

#include "memory_alloc_pure.h"
#include "memory_alloc_xr.h"

#include	"xrsharedmem.h"
#include	"xrMemory_pure.h"

// HACK: ForserX: Хак для установки уровня инициализации переменной в глобальном пространстве
#pragma section(".Hook", read)

BOOL		mem_initialized	= FALSE;
bool		shared_str_initialized	= false;

//fake fix of memory corruptions in multiplayer game :(
XRCORE_API	bool g_allow_heap_min = true;

// Processor specific implementations
extern		pso_MemCopy		xrMemCopy_MMX;
extern		pso_MemCopy		xrMemCopy_x86;
extern		pso_MemFill		xrMemFill_x86;
extern		pso_MemFill32	xrMemFill32_MMX;
extern		pso_MemFill32	xrMemFill32_x86;

xrMemory::xrMemory()
{
	mem_copy	= xrMemCopy_x86;
	mem_fill	= xrMemFill_x86;
	mem_fill32	= xrMemFill32_x86;

#ifndef PURE_ONLY
	if (!!strstr(GetCommandLineA(), "-pure_alloc")) {
		pAlloc = CMemAllocPure::Create();
	} else {
		pAlloc = CMemAllocXRay::Create();
	}
#else
	pAlloc = CMemAllocPure::Create();
#endif
}

void xrMemory::_initialize(BOOL bDebug)
{
	stat_calls = 0;
	stat_counter = 0;

	if (CPU::ID.hasFeature(CPUFeature::MMXExt))
	{
		mem_copy = xrMemCopy_MMX;
		mem_fill = xrMemFill_x86;
		mem_fill32 = xrMemFill32_MMX;
	}
	else {
		mem_copy = xrMemCopy_x86;
		mem_fill = xrMemFill_x86;
		mem_fill32 = xrMemFill32_x86;
	}

	mem_initialized = TRUE;

	g_pStringContainer = xr_new<str_container>();
	shared_str_initialized = true;
	g_pSharedMemoryContainer = xr_new<smem_container>();
}

void xrMemory::_destroy()
{
	xr_delete					(g_pSharedMemoryContainer);
	xr_delete					(g_pStringContainer);

	mem_initialized				= FALSE;
}

void xrMemory::mem_compact()
{
	RegFlushKey(HKEY_CLASSES_ROOT);
	RegFlushKey(HKEY_CURRENT_USER);

	if (g_allow_heap_min) {
		_heapmin();
	}

	HeapCompact(GetProcessHeap(), 0);
	if (g_pStringContainer)			g_pStringContainer->clean();
	if (g_pSharedMemoryContainer)	g_pSharedMemoryContainer->clean();
	if (strstr(Core.Params, "-swap_on_compact")) {
		SetProcessWorkingSetSize(GetCurrentProcess(), size_t(-1), size_t(-1));
	}
}

// xr_strdup
char*			xr_strdup		(const char* string)
{	
	VERIFY	(string);
	u32		len			= u32(xr_strlen(string))+1	;
	char *	memory		= (char*) Memory.mem_alloc(len);
	CopyMemory		(memory,string,len);
	return	memory;
}
wchar_t* xr_strdup(const wchar_t* string) {
	VERIFY(string);
	size_t len = u32(wcslen(string)) + 1;
	wchar_t* memory = new wchar_t[len];
	CopyMemory(memory, string, len);
	return memory;
}

XRCORE_API		BOOL			is_stack_ptr		( void* _ptr)
{
	int			local_value		= 0;
	void*		ptr_refsound	= _ptr;
	void*		ptr_local		= &local_value;
	ptrdiff_t	difference		= (ptrdiff_t)_abs(s64(ptrdiff_t(ptr_local) - ptrdiff_t(ptr_refsound)));
	return		(difference < (512*1024));
}

#pragma init_seg(lib)
__declspec(allocate(".Hook"))
xrMemory Memory;
