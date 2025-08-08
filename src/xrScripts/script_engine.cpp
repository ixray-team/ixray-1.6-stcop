////////////////////////////////////////////////////////////////////////////
//	Module 		: script_engine.cpp
//	Created 	: 01.04.2004
//  Modified 	: 01.04.2004
//	Author		: Dmitriy Iassenev
//	Description : XRay Script Engine
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "script_engine.h"
#include "../xrEngine/IGame_ObjectFactory.h"
#include "../xrCore/xrAddons.h"
#include "script_process.h"
#include "lua_ext.h"

#include "script_callback_ex.h"
#include <luabind/class_info.hpp>

SCRIPTS_API CScriptEngine* g_pScriptEngine = nullptr;

void jit_command(lua_State*, LPCSTR);

CScriptEngine::CScriptEngine			()
{
	m_stack_level			= 0;
	m_reload_modules		= false;
	m_last_no_file_length	= 0;
	*m_last_no_file			= 0;
}

CScriptEngine::~CScriptEngine			()
{
	while (!m_script_processes.empty())
		remove_script_process(m_script_processes.begin()->first);

#ifdef DEBUG
	flush_log					();
#endif // DEBUG
}

void CScriptEngine::unload				()
{
	lua_settop				(lua(),m_stack_level);
	m_last_no_file_length	= 0;
	*m_last_no_file			= 0;
}

int CScriptEngine::lua_panic			(lua_State *L)
{
	g_pScriptEngine->print_stack();
	print_output	(L,"PANIC",LUA_ERRRUN);
	return			(0);
}

void CScriptEngine::lua_error			(lua_State *L)
{
	print_output(L, "", LUA_ERRRUN);

	g_pScriptEngine->on_error	(L);

#if !XRAY_EXCEPTIONS
	Debug.fatal				(DEBUG_INFO,"LUA error: %s",lua_tostring(L,-1));
	g_pScriptEngine->print_stack();
#else
	throw					lua_tostring(L,-1);
#endif
}

int CScriptEngine::lua_pcall_failed(lua_State* L)
{
	g_pScriptEngine->print_stack();
	print_output(L, "", LUA_ERRRUN);
	g_pScriptEngine->on_error(L);

#if !XRAY_EXCEPTIONS
	Debug.fatal(DEBUG_INFO, "LUA error: %s", lua_isstring(L, -1) ? lua_tostring(L, -1) : "");
#endif
	if (lua_isstring(L, -1))
		lua_pop(L, 1);

	return (LUA_ERRRUN);
}

void lua_cast_failed(lua_State *L, LUABIND_TYPE_INFO info)
{
	g_pScriptEngine->print_stack();
	CScriptEngine::print_output	(L,"",LUA_ERRRUN);

	Debug.fatal(DEBUG_INFO,"LUA error: cannot cast lua value to %s",info->name());
}

void CScriptEngine::setup_callbacks		()
{
	{
#ifdef LUABIND_NO_EXCEPTIONS
		luabind::set_error_callback		(CScriptEngine::lua_error);
#endif

#if 0
		luabind::set_pcall_callback
		(
			[](lua_State* L) ->void
			{  
				lua_pushcfunction(L, CScriptEngine::lua_pcall_failed);
			}
		);
#endif
	}

#ifdef LUABIND_NO_EXCEPTIONS
	luabind::set_cast_failed_callback	(lua_cast_failed);
#endif

	lua_atpanic							(lua(),CScriptEngine::lua_panic);
//	luabind::disable_super_deprecation();
}

#ifdef DEBUG
#	include "script_thread.h"
void CScriptEngine::lua_hook_call		(lua_State *L, lua_Debug *dbg)
{
	if (g_pScriptEngine->current_thread())
		g_pScriptEngine->current_thread()->script_hook(L,dbg);
}
#endif

int auto_load(lua_State* L)
{
	int StackSize = lua_gettop(L);
	bool IsTable = lua_istable(L, 1);

	if ((StackSize < 2) || !IsTable || !lua_isstring(L, 2)) {
		lua_pushnil(L);
		return		(1);
	}

	xr_string file_name_space = lua_tostring(L, 2);

	if (g_pScriptEngine->xray_scripts.contains(file_name_space))
	{
		g_pScriptEngine->process_file_if_exists(file_name_space.c_str(), false);
		lua_rawget(L, 1);
	}
	else
	{
		lua_pushnil(L);
	}

	return (1);
}


void CScriptEngine::setup_auto_load		()
{
	luaL_newmetatable					(lua(),"XRAY_AutoLoadMetaTable");
	lua_pushstring						(lua(),"__index");
	lua_pushcfunction					(lua(), auto_load);
	lua_settable						(lua(),-3);
	lua_pushstring 						(lua(),"_G"); 
	lua_gettable 						(lua(),LUA_GLOBALSINDEX); 
	luaL_getmetatable					(lua(),"XRAY_AutoLoadMetaTable");
	lua_setmetatable					(lua(),-2);
	//. ??????????
	// lua_settop							(lua(),-0);

	//Alun: Allow directory structuring for scripts
	xray_scripts.clear();

	FS_FileSet fset;
	FS.file_list(fset, "$game_scripts$", FS_ListFiles, "*.script");
	FS_FileSetIt fit = fset.begin();
	FS_FileSetIt fit_e = fset.end();

	for (; fit != fit_e; ++fit)
	{
		string_path	fn1, fn2;
		_splitpath((*fit).name.c_str(), 0, fn1, fn2, 0);

		FS.update_path(fn1, "$game_scripts$", fn1);
		xr_strconcat(fn1, fn1, fn2, ".script");

		if (!xray_scripts.contains(xr_string(fn2)))
		{
			xray_scripts[fn2] = fn1;
			continue;
		}

		Msg("! script already exists by namespace %s", fn2);
	}
}

void CScriptEngine::init()
{
	CScriptStorage::reinit();

	luabind::open(lua());
	luabind::bind_class_info(lua());
	setup_callbacks();
	g_object_factory->export_classes(lua());
	setup_auto_load();

#ifdef DEBUG
	lua_sethook(lua(), lua_hook_call, LUA_MASKLINE | LUA_MASKCALL | LUA_MASKRET, 0);
#endif

	bool								save = m_reload_modules;
	m_reload_modules = true;
	process_file_if_exists("_g", false);
	m_reload_modules = save;

	register_script_classes();
	g_object_factory->register_script();

#ifdef XRGAME_EXPORTS
	load_common_scripts();
#endif
	m_stack_level = lua_gettop(lua());

#ifdef xrScripts_EXPORTS
	if (IsLDBGAttached)
	{
		DebbugerAttach();
	}

	if (g_pAddonsManager != nullptr)
	{
		for (auto& Addon : g_pAddonsManager->Addons)
		{
			if (Addon.ScriptInit.size() == 0)
				continue;

			LoadScriptToGlobal(lua(), Addon.ScriptInit.c_str(), false);
		}
	}

#endif

	add_script_process(ScriptEngine::eScriptProcessorHelper, new CScriptProcess("ImHelper", ""));
}

void CScriptEngine::remove_script_process	(const EScriptProcessors &process_id)
{
	CScriptProcessStorage::iterator	I = m_script_processes.find(process_id);
	if (I != m_script_processes.end()) {
		xr_delete						((*I).second);
		m_script_processes.erase		(I);
	}
}

void CScriptEngine::load_common_scripts()
{
#ifdef DBG_DISABLE_SCRIPTS
	return;
#endif
	string_path		S;
	FS.update_path	(S,"$game_config$","script.ltx");
	CInifile		*l_tpIniFile = new CInifile(S);
	R_ASSERT		(l_tpIniFile);
	if (!l_tpIniFile->section_exist("common")) {
		xr_delete			(l_tpIniFile);
		return;
	}

	if (l_tpIniFile->line_exist("common","script")) {
		LPCSTR			caScriptString = l_tpIniFile->r_string("common","script");
		u32				n = _GetItemCount(caScriptString);
		string256		I;
		for (u32 i=0; i<n; ++i) {
			process_file(_GetItem(caScriptString,i,I));
			xr_strcat	(I,"_initialize");
			if (object("_G",I,LUA_TFUNCTION)) {
//				lua_dostring			(lua(),xr_strcat(I,"()"));
				luabind::functor<void>	f;
				R_ASSERT				(functor(I,f));
				f						();
			}
		}
	}

	xr_delete			(l_tpIniFile);
}

shared_str ParseFolder(LPCSTR file_name, FS_FileSet& SET)
{
	bool Finded = false;

	string_path path_to_file;

	for (auto FOLDER : SET)
	{
		// GET NAME PATH
		string128 name_file = { 0 }; 
		
		// PATCH BASE
		FS.update_path(path_to_file, "$game_scripts$", FOLDER.name.c_str());
		sprintf_s(name_file, "%s.script", file_name);
		xr_strcat(path_to_file, name_file);
		
		// Check FOR FOLDERS
		FS_FileSet test;
		FS.file_list(test, path_to_file, FS_ListFolders);
		if (test.size() > 0)
			ParseFolder(file_name, test);

		if (FS.exist(path_to_file))
 			Finded = true; break;
 	}

	if (Finded)
		return path_to_file;
	else
		return nullptr;
}

void CScriptEngine::process_file_if_exists(LPCSTR file_name, bool warn_if_not_exist)
{
	if (!*file_name)
		return;

	if (!m_reload_modules && namespace_loaded(file_name))
		return;

	script_list_type::iterator it = xray_scripts.find(xr_string(file_name));
	if (it != xray_scripts.end())
	{
		Msg("* loading script %s.script", file_name);
		m_reload_modules = false;

		load_file_into_namespace(it->second.c_str(), strcmp(file_name, "_g") == 0 ? "_G" : file_name);
		return;
	}

	if (warn_if_not_exist)
		Msg("Variable %s not found; No script by this name exists, either.", file_name);

	return;

}

void CScriptEngine::process_file	(LPCSTR file_name)
{
	process_file_if_exists	(file_name,true);
}

void CScriptEngine::process_file	(LPCSTR file_name, bool reload_modules)
{
	m_reload_modules		= reload_modules;
	process_file_if_exists	(file_name,true);
	m_reload_modules		= false;
}

void CScriptEngine::register_script_classes		()
{
#ifdef DBG_DISABLE_SCRIPTS
	return;
#endif
	string_path					S;
	FS.update_path				(S,"$game_config$","script.ltx");
	CInifile					*l_tpIniFile = new CInifile(S);
	R_ASSERT					(l_tpIniFile);

	if (!l_tpIniFile->section_exist("common")) {
		xr_delete				(l_tpIniFile);
		return;
	}

	m_class_registrators		= READ_IF_EXISTS(l_tpIniFile,r_string,"common","class_registrators","");
	xr_delete					(l_tpIniFile);

	u32 n = _GetItemCount(*m_class_registrators);
	string256 I;

	for (u32 i=0; i<n; ++i) 
	{
		_GetItem(*m_class_registrators,i,I);
		g_object_factory->init_script(I);
	}
}

bool CScriptEngine::function_object(LPCSTR function_to_call, luabind::object &object, int type)
{
	if (!xr_strlen(function_to_call))
		return				(false);

	string256				name_space, function;

	parse_script_namespace	(function_to_call,name_space,sizeof(name_space),function,sizeof(function));
	if (xr_strcmp(name_space,"_G")) {
		LPSTR				file_name = strchr(name_space,'.');
		if (!file_name)
			process_file	(name_space);
		else {
			*file_name		= 0;
			process_file	(name_space);
			*file_name		= '.';
		}
	}

	if (!this->object(name_space,function,type))
		return				(false);

	luabind::object			lua_namespace	= this->name_space(name_space);
	object					= lua_namespace[function];
	return					(true);
}

bool CScriptEngine::no_file_exists	(LPCSTR file_name, u32 string_length)
{
	if (m_last_no_file_length != string_length)
		return				(false);

	return					(!memcmp(m_last_no_file,file_name,string_length*sizeof(char)));
}

void CScriptEngine::add_no_file		(LPCSTR file_name, u32 string_length)
{
	m_last_no_file_length	= string_length;
	CopyMemory				(m_last_no_file,file_name,(string_length+1)*sizeof(char));
}

void CScriptEngine::collect_all_garbage	()
{
	lua_gc					(lua(),LUA_GCCOLLECT,0);
	lua_gc					(lua(),LUA_GCCOLLECT,0);
}

void CScriptEngine::on_error(lua_State* state)
{
}