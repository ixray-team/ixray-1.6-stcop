////////////////////////////////////////////////////////////////////////////
//	Module 		: xrGame.cpp
//	Created 	: 07.01.2001
//  Modified 	: 27.05.2004
//	Author		: Aleksandr Maksimchuk and Oles' Shyshkovtsov
//	Description : Defines the entry point for the DLL application.
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "DynamicWallmarkZone.h"
#include "object_factory.h"
#include "../../xrUI/xrUIXmlParser.h"
#include "../xrEngine/xr_level_controller.h"

#include "ImUtils/ImUtils.h"

void CCC_RegisterCommands	();
void RegisterExpressionDelegates();

CInifile* pGameGlobals = nullptr;

CImGuiRequestManager g_imgui_editor_request_manager;
void LoadCallbackGlobals(bool& flag, const char*& value, const char* section)
{
	flag = pGameGlobals->line_exist("callbacks", section);
	if (flag)
	{
		value = pGameGlobals->r_string("callbacks", section);
		flag = (value != nullptr && value[0] != '\0');
	}
};

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


	str.append("test");

	for (auto it : str)
	{
		char a = it;
	}

	str.append("123");

	auto substr = str.substr();
	assert(substr == str);
	auto substr2 = str.substr(3);
	assert(substr2 == "t123");

	auto index = substr2.find("12");
	assert(index == 1);
	index = substr2.find("3");
	assert(index == 3);
}
#endif

extern "C" 
{
	DLL_API void __cdecl xrGameInitialize()
	{
		string_path GameGlobals = {};
		FS.update_path(GameGlobals, _game_config_, "game_global.ltx");
		pGameGlobals = new CInifile(GameGlobals);

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
	}

	DLL_API void __cdecl xrGameShutdown()
	{
		AllEditors_Shutdown();
		g_imgui_editor_request_manager.requests.wait();
		luabind::allocator = nullptr;
		xr_delete(pGameGlobals);
	}
	
	DLL_API void __cdecl xrGameRenderPreDestroy()
	{
		CinemaFilmMaker_Shutdown();
		CDynamicWallmarkRegistry::Instance().ClearWallmarks();
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

	// Level Editor: the ltx schema of the IniRead classes and the class ids of their sections
	DLL_API bool		__cdecl	xrBakeIniSchema(const char* Path)
	{
		IWriter* Writer = FS.w_open(Path);
		if (Writer == nullptr)
		{
			return false;
		}

		Writer->w_string("# IX-Ray ltx schema: S schema, F field, O schemas of a class id");

		u32 Count = 0;
		const XReflect::SIniSchema* Schemas = XReflect::GetIniSchemas(Count);
		for (u32 Index = 0; Index < Count; ++Index)
		{
			const XReflect::SIniSchema& Schema = Schemas[Index];
			Writer->w_string((xr_string("S\t") + Schema.Name + "\t" + Schema.Owner + "\t" + Schema.Bases).c_str());
			for (u32 Field = 0; Field < Schema.FieldCount; ++Field)
			{
				const XReflect::SIniField& Info = Schema.Fields[Field];
				Writer->w_string((xr_string("F\t") + Schema.Name + "\t" + Info.Key + "\t" + Info.Type + "\t" + (Info.Required ? "1" : "0") + "\t" + Info.If + "\t" + (Info.Nested ? "1" : "0")).c_str());
			}
		}

		// No object is created: the factory items know which schemas their client class reads
		for (const CObjectItemAbstract* Item : object_factory().items())
		{
			if (Item->ini_schemas == nullptr)
			{
				continue;
			}

			xr_vector<const char*> Names;
			Item->ini_schemas(Names);

			string16 ClassId;
			CLSID2TEXT(Item->clsid(), ClassId);
			xr_string Text = ClassId;
			while (!Text.empty() && Text.back() == ' ')
			{
				Text.pop_back();
			}

			for (const char* Name : Names)
			{
				Writer->w_string(("O\t" + Text + "\t" + Name).c_str());
			}
		}

		FS.w_close(Writer);
		return true;
	}
}
