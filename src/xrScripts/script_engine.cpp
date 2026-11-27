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
#include <luabind/error.hpp>

#ifdef IXRAY_PROFILER_TRACY
	#define TRACY_CALLSTACK 8
		#include <tracy/TracyLua.hpp>
	#undef TRACY_CALLSTACK
#endif

SCRIPTS_API CScriptEngine* g_pScriptEngine = nullptr;

namespace ScriptEngineDetail
{
	constexpr char RegistryKey[] = "ixray_script_engine";

	CScriptEngine* ResolveEngine(lua_State* L)
	{
		if (L == nullptr)
			return nullptr;
		lua_pushlightuserdata(L, const_cast<char*>(RegistryKey));
		lua_rawget(L, LUA_REGISTRYINDEX);
		CScriptEngine* engine = static_cast<CScriptEngine*>(lua_touserdata(L, -1));
		lua_pop(L, 1);
		return engine != nullptr && engine->lua() == L ? engine : nullptr;
	}
}

CScriptEngine* CScriptEngine::EngineFromState(lua_State* L)
{
	return ScriptEngineDetail::ResolveEngine(L);
}

void jit_command(lua_State*, const char*);

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
	if (CScriptEngine* engine = EngineFromState(L))
		engine->print_stack();
	print_output	(L,"PANIC",LUA_ERRRUN);
	return			(0);
}

void LogScriptError(lua_State* state)
{
	const char* message = lua_isstring(state, -1) ? lua_tostring(state, -1) : lua_typename(state, lua_type(state, -1));
	Msg("! [LUA] protected call failed: %s", message != nullptr ? message : "<non-string error>");
	const int top = lua_gettop(state);
	lua_getglobal(state, "debug");
	if (lua_istable(state, -1))
	{
		lua_getfield(state, -1, "traceback");
		if (lua_isfunction(state, -1))
		{
			lua_pushvalue(state, top);
			lua_pushinteger(state, 2);
			if (lua_pcall(state, 2, 1, 0) == 0 && lua_isstring(state, -1))
				Msg("! [LUA] traceback: %s", lua_tostring(state, -1));
		}
	}
	lua_settop(state, top);
}

void CScriptEngine::lua_error			(lua_State *L)
{
	LogScriptError(L);
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
	LogScriptError(L);
	g_pScriptEngine->on_error(L);
	return 1;
}

void lua_cast_failed(lua_State *L, LUABIND_TYPE_INFO info)
{
	g_pScriptEngine->print_stack();
	CScriptEngine::print_output	(L,"",LUA_ERRRUN);

	Debug.fatal(DEBUG_INFO,"LUA error: cannot cast lua value to %s",info->name());
}

int CScriptEngine::lua_pcall_message(lua_State* L)
{
	const char* message = lua_isstring(L, 1) ? lua_tostring(L, 1) : "(error object is not a string)";
	if (Core.ParamsData.test(ECoreParams::use_callstack))
		g_pScriptEngine->print_stack();
	lua_getglobal(L, "debug");
	if (lua_istable(L, -1))
	{
		lua_getfield(L, -1, "traceback");
		if (lua_isfunction(L, -1))
		{
			lua_pushstring(L, message);
			lua_pushinteger(L, 2);
			if (!lua_pcall(L, 2, 1, 0) && lua_isstring(L, -1))
				return 1;
		}
	}
	lua_settop(L, 1);
	if (!lua_isstring(L, 1))
		lua_pushstring(L, message);
	return 1;
}

void CScriptEngine::setup_callbacks()
{
	luabind::set_pcall_callback(CScriptEngine::lua_pcall_message);
#ifdef LUABIND_NO_EXCEPTIONS
	luabind::set_error_callback(CScriptEngine::lua_error);
	luabind::set_cast_failed_callback(lua_cast_failed);
#endif

	lua_pushlightuserdata(lua(), const_cast<char*>(ScriptEngineDetail::RegistryKey));
	lua_pushlightuserdata(lua(), this);
	lua_rawset(lua(), LUA_REGISTRYINDEX);
	lua_atpanic(lua(), CScriptEngine::lua_panic);
}

#ifdef DEBUG
#	include "script_thread.h"
void CScriptEngine::lua_hook_call		(lua_State *L, lua_Debug *dbg)
{
	CScriptEngine* engine = EngineFromState(L);
	if (engine == nullptr)
		return;
	CScriptThread* thread = engine->current_thread();
	if (thread != nullptr)
		thread->script_hook(L, dbg);
}
#endif

int auto_load(lua_State* L)
{
	int StackSize = lua_gettop(L);
	bool IsTable = lua_istable(L, 1);

	if ((StackSize < 2) || !IsTable || !lua_isstring(L, 2))
	{
		lua_pushnil(L);
		return 1;
	}

	xr_string file_name_space = lua_tostring(L, 2);
	CScriptEngine* engine = static_cast<CScriptEngine*>(lua_touserdata(L, lua_upvalueindex(1)));

	if (engine != nullptr && engine->xray_scripts.contains(file_name_space))
	{
		engine->process_file_if_exists(file_name_space.c_str(), false);
		lua_rawget(L, 1);
	}
	else
	{
		lua_pushnil(L);
	}

	return 1;
}

void CScriptEngine::setup_auto_load		()
{
	luaL_newmetatable					(lua(),"XRAY_AutoLoadMetaTable");
	lua_pushstring						(lua(),"__index");
	lua_pushlightuserdata				(lua(), this);
	lua_pushcclosure					(lua(), auto_load, 1);
	lua_settable						(lua(),-3);
	lua_pushstring 						(lua(),"_G"); 
	lua_gettable 						(lua(),LUA_GLOBALSINDEX); 
	luaL_getmetatable					(lua(),"XRAY_AutoLoadMetaTable");
	lua_setmetatable					(lua(),-2);
	//. ??????????

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
	PROF_EVENT("CScriptEngine::init");
	if (g_object_factory != nullptr)
		g_object_factory->release_script_engine(this);
	for (int Iter = ScriptEngine::EScriptProcessors::eScriptProcessorLevel; Iter <= ScriptEngine::EScriptProcessors::eScriptProcessorHelper; Iter++)
	{
		if (CScriptProcess* ScriptProces = script_process((ScriptEngine::EScriptProcessors)Iter))
		{
			ScriptProces->DestroyScripts();
		}
	}

	{
		PROF_EVENT("ScriptEngine: new VM");
		CScriptStorage::reinit();
	}

	{
		PROF_EVENT("ScriptEngine: export");
		luabind::open(lua());
#ifdef IXRAY_PROFILER_TRACY
		tracy::LuaRegister(lua());
#endif


		luabind::bind_class_info(lua());
		setup_callbacks();
		g_object_factory->export_classes(lua());
		setup_auto_load();
	}

#ifdef DEBUG
	lua_sethook(lua(), lua_hook_call, LUA_MASKLINE | LUA_MASKCALL | LUA_MASKRET, 0);
#endif

	bool								save = m_reload_modules;
	m_reload_modules = true;
	{
		PROF_EVENT("ScriptEngine: _g");
		process_file_if_exists("_g", false);
	}
	m_reload_modules = save;

	{
		PROF_EVENT("ScriptEngine: register classes");
		register_script_classes();
		g_object_factory->register_script();
	}

#ifdef XRGAME_EXPORTS
	{
		PROF_EVENT("ScriptEngine: common scripts");
		load_common_scripts();
	}
#endif
	m_stack_level = lua_gettop(lua());

#ifdef xrScripts_EXPORTS
	if (IsLDBGAttached || strstr(Core.Params, "-ldbg"))
	{
		DebbugerAttach();
	}

	if (GAddonsManager != nullptr)
	{
		PROF_EVENT("ScriptEngine: addon init scripts");
		for (auto& Addon : GAddonsManager->Addons)
		{
			if (Addon.ScriptInit.size() == 0)
			{
				continue;
			}

			LoadScriptToGlobal(lua(), Addon.ScriptInit.c_str(), false);
		}
	}

#endif

	add_script_process(ScriptEngine::eScriptProcessorHelper, new CScriptProcess(*this, "ImHelper", ""));
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
	string_path S;
	FS.update_path(S, _game_config_, "script.ltx");
	CInifile* l_tpIniFile = new CInifile(S);
	R_ASSERT(l_tpIniFile);

	if (!l_tpIniFile->section_exist("common"))
	{
		xr_delete(l_tpIniFile);
		return;
	}

	if (l_tpIniFile->line_exist("common", "script"))
	{
		const char* caScriptString = l_tpIniFile->r_string("common", "script");
		u32 n = _GetItemCount(caScriptString);
		string256 I;
		for (u32 i = 0; i < n; ++i)
		{
			process_file(_GetItem(caScriptString, i, I));
			xr_strcat(I, "_initialize");
			if (object("_G", I, LUA_TFUNCTION))
			{
				luabind::functor<void>	f;
				R_ASSERT(functor(I, f));
				f();
			}
		}
	}

	xr_delete(l_tpIniFile);
}

shared_str ParseFolder(const char* file_name, FS_FileSet& SET)
{
	bool Finded = false;

	string_path path_to_file;

	for (const FS_File& FOLDER : SET)
	{
		// GET NAME PATH
		string128 name_file = { 0 }; 
		
		// PATCH BASE
		FS.update_path(path_to_file, "$game_scripts$", FOLDER.name.c_str());
		xr_sprintf(name_file, "%s.script", file_name);
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

void CScriptEngine::process_file_if_exists(const char* file_name, bool warn_if_not_exist)
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

void CScriptEngine::process_file	(const char* file_name)
{
	process_file_if_exists	(file_name,true);
}

void CScriptEngine::process_file	(const char* file_name, bool reload_modules)
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
	FS.update_path				(S,_game_config_,"script.ltx");
	CInifile					*l_tpIniFile = new CInifile(S);
	R_ASSERT					(l_tpIniFile);

	if (!l_tpIniFile->section_exist("common")) {
		xr_delete				(l_tpIniFile);
		return;
	}

	m_class_registrators		= l_tpIniFile->read_if_exists<str_c>("common","class_registrators","");
	xr_delete					(l_tpIniFile);

	u32 n = _GetItemCount(*m_class_registrators);
	string256 I;

	for (u32 i=0; i<n; ++i) 
	{
		_GetItem(*m_class_registrators,i,I);
		g_object_factory->init_script(I);
	}
}

bool CScriptEngine::function_object(const char* function_to_call, luabind::object &object, int type)
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

bool CScriptEngine::no_file_exists	(const char* file_name, u32 string_length)
{
	if (m_last_no_file_length != string_length)
		return				(false);

	return					(!memcmp(m_last_no_file,file_name,string_length*sizeof(char)));
}

void CScriptEngine::add_no_file		(const char* file_name, u32 string_length)
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