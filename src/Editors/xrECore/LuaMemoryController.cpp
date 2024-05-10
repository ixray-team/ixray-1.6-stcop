
#include "stdafx.h"
#include "..\..\XrCore\xrCore.h"
#include "luabind\luabind.hpp"
static  void* __cdecl luabind_allocator(void* context, void const* pointer, size_t size)
{
	if (!size) {
		void* non_const_pointer = const_cast<void*>(pointer);
		xr_free(non_const_pointer);
		return	(0);
	}
	void* non_const_pointer = const_cast<void*>(pointer);

	return		(xr_realloc(non_const_pointer, size));
}
static struct SetupLuabindAllocator
{
	SetupLuabindAllocator()
	{
		luabind::allocator = &luabind_allocator;
	}
}
LSetupLuabindAllocator;