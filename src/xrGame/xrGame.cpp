////////////////////////////////////////////////////////////////////////////
//	Module 		: xrGame.cpp
//	Created 	: 07.01.2001
//  Modified 	: 27.05.2004
//	Author		: Aleksandr Maksimchuk and Oles' Shyshkovtsov
//	Description : Defines the entry point for the DLL application.
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "object_factory.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../xrEngine/xr_level_controller.h"

void CCC_RegisterCommands	();
void RegisterExpressionDelegates();

CInifile* pGameGlobals = nullptr;

extern void RegisterImGuiInGame();


extern "C" 
{
	DLL_API void __cdecl xrGameInitialize()
	{
		CCC_RegisterCommands();
		// keyboard binding
		CCC_RegisterInput();

		RegisterExpressionDelegates();

#ifdef DEBUG_DRAW
		RegisterImGuiInGame();
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
