////////////////////////////////////////////////////////////////////////////
//	Module 		: script_thread.cpp
//	Created 	: 19.09.2003
//  Modified 	: 29.06.2004
//	Author		: Dmitriy Iassenev
//	Description : Script thread class
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
extern "C" {
	#include "lua/lua.h"
};
#include "script_engine.h"
#include "script_thread.h"

#define LUABIND_HAS_BUGS_WITH_LUA_THREADS

const LPCSTR main_function = "console_command_run_string_main_thread_function";

CScriptThread::CScriptThread(LPCSTR caNamespaceName, bool do_string, bool reload)
{
	m_virtual_machine = 0;
	m_active = false;

	try
	{
		string256 S;
		if (!do_string) {
			m_script_name = caNamespaceName;
			g_pScriptEngine->process_file(caNamespaceName, reload);
		}
		else
		{
			m_script_name = "console command";
			xr_sprintf(S, "function %s()\n%s\nend\n", main_function, caNamespaceName);
			int l_iErrorCode = luaL_loadbuffer(g_pScriptEngine->lua(), S, xr_strlen(S), "@console_command");

			if (!l_iErrorCode)
			{
				l_iErrorCode = lua_pcall(g_pScriptEngine->lua(), 0, 0, 0);

				if (l_iErrorCode) 
				{
					g_pScriptEngine->print_output(g_pScriptEngine->lua(), *m_script_name, l_iErrorCode);
					g_pScriptEngine->on_error(g_pScriptEngine->lua());
					return;
				}
			}
			else
			{
				g_pScriptEngine->print_output(g_pScriptEngine->lua(), *m_script_name, l_iErrorCode);
				g_pScriptEngine->on_error(g_pScriptEngine->lua());
				return;
			}
		}

		m_virtual_machine = lua_newthread(g_pScriptEngine->lua());

		VERIFY2(lua(), "Cannot create new Lua thread");

#ifdef DEBUG
		lua_sethook(lua(), CScriptEngine::lua_hook_call, LUA_MASKLINE | LUA_MASKCALL | LUA_MASKRET, 0);
#endif

		if (!do_string)
			xr_sprintf(S, "%s.main()", caNamespaceName);
		else
			xr_sprintf(S, "%s()", main_function);

		if (!g_pScriptEngine->load_buffer(lua(), S, xr_strlen(S), "@_thread_main"))
			return;

		m_active = true;
	}
	catch (...)
	{
		m_active = false;
	}
}

CScriptThread::~CScriptThread()
{
#ifdef DEBUG
	Msg						("* Destroying script thread %s",*m_script_name);
#endif
	try 
	{
#ifndef LUABIND_HAS_BUGS_WITH_LUA_THREADS
		luaL_unref			(g_pScriptEngine->lua(),LUA_REGISTRYINDEX,m_thread_reference);
#endif
	}
	catch(...) {
	}
}

bool CScriptThread::update()
{
	if (!m_active)
		R_ASSERT2		(false,"Cannot resume dead Lua thread!");

	try {
		g_pScriptEngine->current_thread	(this);
		
		int					l_iErrorCode = lua_resume(lua(),0);
		
		if (l_iErrorCode && (l_iErrorCode != LUA_YIELD))
		{
			g_pScriptEngine->print_output(lua(), *script_name(), l_iErrorCode);
			g_pScriptEngine->on_error(g_pScriptEngine->lua());
			print_stack(lua());
			m_active = false;
		}
		else 
		{
			if (l_iErrorCode != LUA_YIELD) 
			{
				if (m_current_stack_level) 
				{
					g_pScriptEngine->print_output	(lua(),*script_name(),l_iErrorCode);
					g_pScriptEngine->on_error		(g_pScriptEngine->lua());
//					print_stack		(lua());
				}
				m_active	= false;
				g_pScriptEngine->script_log	(ScriptStorage::eLuaMessageTypeInfo,"Script %s is finished!",*m_script_name);
			}
			else {
				VERIFY2		(!lua_gettop(lua()),"Do not pass any value to coroutine.yield()!");
			}
		}
		
		g_pScriptEngine->current_thread	(0);
	}
	catch(...) {
		g_pScriptEngine->current_thread	(0);
		m_active		= false;
	}
	return				(m_active);
}
