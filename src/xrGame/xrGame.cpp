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

CImGuiEditorsGlobalState g_imgui_editors_state;

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

		if (g_imgui_editors_state.is_thread_started == false && !Device.IsEditorMode())
		{
			g_imgui_editors_state.worker_thread = std::thread(&AllEditorsAndTools_WorkerThread);
			g_imgui_editors_state.is_thread_started = true;
		}

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
		if (g_imgui_editors_state.is_thread_started)
		{
			ime_request_t req;

			req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
			req.request_type = static_cast<u32>(CImGuiTextureEditor::eRequestType::kWriteSettings);

			g_imgui_editors_state.requests.push(req);

			req.editor_type = static_cast<u32>(eImGuiEditorType::kTextureEditor);
			req.request_type = static_cast<u32>(CImGuiTextureEditor::eRequestType::kUnloadResources);

			g_imgui_editors_state.requests.push(req);

			req.editor_type = static_cast<u32>(eImGuiEditorType::kNoEditor);
			req.request_type = 0; // means we want to shutdown our thread
			
			g_imgui_editors_state.requests.push(req);

			g_imgui_editors_state.worker_thread.join();
		}
	}
	
	DLL_API void __cdecl xrGameRenderPreDestroy()
	{
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
}
