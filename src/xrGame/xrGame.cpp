////////////////////////////////////////////////////////////////////////////
//	Module 		: xrGame.cpp
//	Created 	: 07.01.2001
//  Modified 	: 27.05.2004
//	Author		: Aleksandr Maksimchuk and Oles' Shyshkovtsov
//	Description : Defines the entry point for the DLL application.
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "object_factory.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../xrEngine/xr_level_controller.h"

void CCC_RegisterCommands	();
void RegisterExpressionDelegates();

CInifile* pGameGlobals = nullptr;

extern void RegisterImGuiInGame();
static LPVOID __cdecl luabind_allocator(
	luabind::memory_allocation_function_parameter const,
	void const* const pointer,
	size_t const size
)
{
	if (!size) 
	{
		LPVOID	non_const_pointer = const_cast<LPVOID>(pointer);
		xr_free(non_const_pointer);
		return	(0);
	}

	if (!pointer) 
	{
		return	(Memory.mem_alloc(size));
	}

	LPVOID non_const_pointer = const_cast<LPVOID>(pointer);
	return (Memory.mem_realloc(non_const_pointer, size));
}

void setup_luabind_allocator		()
{
	if (!Device.IsEditorMode())
	{
		luabind::allocator = &luabind_allocator;
		luabind::allocator_parameter = 0;
	}
}

#ifdef DEBUG
void unit_test_stack_string()
{
	stack_string<char, 10> str;

	assert(str.empty());
	static_assert(str.max_size() == sizeof(char[10])); // real compile-time assert ^^
	assert(str.max_size() == sizeof(char[10]));
	assert(str.size() == 0);
	assert(str.c_str());
	assert(str.data());

}
#endif

extern "C" 
{
	DLL_API void __cdecl xrGameInitialize()
	{
		CCC_RegisterCommands();
		// keyboard binding
		CCC_RegisterInput();
		setup_luabind_allocator	();
		RegisterExpressionDelegates();

#ifdef DEBUG_DRAW
		RegisterImGuiInGame();
#endif

#ifdef DEBUG
		unit_test_stack_string();
#endif

		string_path GameGlobals = {};
		FS.update_path(GameGlobals, "$game_config$", "game_global.ltx");
		pGameGlobals = new CInifile(GameGlobals);
	}

	DLL_API DLL_Pure* __cdecl xrFactory_Create(CLASS_ID clsid)
	{
		DLL_Pure* object = object_factory().client_object(clsid);

#ifdef DEBUG
		if (!object)
			return			(0);
#endif

		object->CLS_ID = clsid;
		return				(object);
	}

	DLL_API void		__cdecl	xrFactory_Destroy(DLL_Pure* O)
	{
		xr_delete(O);
	}
}
