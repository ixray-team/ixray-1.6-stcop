////////////////////////////////////////////////////////////////////////////
//	Module 		: script_binder.cpp
//	Created 	: 26.03.2004
//  Modified 	: 26.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Script objects binder
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "ai_space.h"
#include "../xrScripts/script_engine.h"
#include "script_binder.h"
#include "xrServer_Objects_ALife.h"
#include "script_binder_object.h"
#include "script_game_object.h"
#include "GameObject.h"
#include "Level.h"

#if !defined(MASTER_GOLD) && defined(IXR_WINDOWS) && (defined(_MSC_VER) || (defined(__clang__) && defined(_MSC_EXTENSIONS)))
#include <windows.h> // for EXCEPTION_ACCESS_VIOLATION
#include <excpt.h>

int ex_filter(unsigned int code, struct _EXCEPTION_POINTERS *ep)
{
	if (IsDebuggerPresent())
	{
		DebugBreak();
	}
	ProcessStackTrace(ep);
	return EXCEPTION_EXECUTE_HANDLER;
}

class SEHExceptionScripts
{
public:
	EXCEPTION_POINTERS* info;
	u32 code;

	SEHExceptionScripts(u32 c, EXCEPTION_POINTERS* i) : info(i), code(c)
	{
		ex_filter(code, info);
	}
};

void SEH_translator_Scripts(u32 code, _EXCEPTION_POINTERS* info)
{
	throw SEHExceptionScripts(code, info);	
}

static std::atomic_bool g_bScriptsSEHInited = false;

#define ALLOW_SEH_EXCEPTIONS

#endif

CScriptBinder::CScriptBinder		()
{
	init					();
}

CScriptBinder::~CScriptBinder		()
{
	VERIFY					(!m_object);
}

void CScriptBinder::init			()
{
	m_object				= 0;
}

void CScriptBinder::clear			()
{
#ifdef ALLOW_SEH_EXCEPTIONS
	if (!g_bScriptsSEHInited)
	{
		_set_se_translator(SEH_translator_Scripts);
		g_bScriptsSEHInited = true;
	}
	try
#endif
	{
		xr_delete			(m_object);
#ifdef ALLOW_SEH_EXCEPTIONS
	} catch(...)
	{
		m_object = nullptr;
#endif
	}
	init					();
}

void CScriptBinder::reinit			()
{
	if (m_object) {
#ifdef ALLOW_SEH_EXCEPTIONS
		if (!g_bScriptsSEHInited)
		{
			_set_se_translator(SEH_translator_Scripts);
			g_bScriptsSEHInited = true;
		}
#endif
		try
		{
			m_object->reinit	();
		} catch(...)
		{
			clear			();
		}
	}
}

void CScriptBinder::Load			(const char* section)
{
}

void CScriptBinder::reload(const char* section)
{
	PROF_EVENT("CScriptBinder::reload");

	VERIFY(!m_object);
	auto func_name = pSettings->r_string_nullable(section, "script_binding");
	if (!func_name)
	{
		return;
	}

	luabind::functor<void>	lua_function;
	if (!ai().script_engine().functor(func_name, lua_function))
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "function %s is not loaded!", func_name);
		return;
	}

	CGameObject* game_object = smart_cast<CGameObject*>(this);

#ifdef ALLOW_SEH_EXCEPTIONS
	if (!g_bScriptsSEHInited)
	{
		_set_se_translator(SEH_translator_Scripts);
		g_bScriptsSEHInited = true;
	}
#endif
	try
	{
		auto script_obj = game_object ? game_object->lua_game_object() : nullptr;
		lua_function(script_obj);
	}
	catch (...)
	{
		clear();
		return;
	}

	if (m_object)
	{
#ifdef ALLOW_SEH_EXCEPTIONS
		if (!g_bScriptsSEHInited)
		{
			_set_se_translator(SEH_translator_Scripts);
			g_bScriptsSEHInited = true;
		}
#endif
		try
		{
			m_object->reload(section);
		}
		catch (...)
		{
			clear();
		}
	}
}

bool CScriptBinder::net_Spawn(CSE_Abstract* DC)
{
	PROF_EVENT("CScriptBinder::net_Spawn");
	CSE_Abstract* abstract = (CSE_Abstract*)DC;
	CSE_ALifeObject* object = smart_cast<CSE_ALifeObject*>(abstract);

	if (object && m_object)
	{
#ifdef ALLOW_SEH_EXCEPTIONS
		if (!g_bScriptsSEHInited)
		{
			_set_se_translator(SEH_translator_Scripts);
			g_bScriptsSEHInited = true;
		}
#endif
		try
		{
			return m_object->net_Spawn(object);
		}
		catch (...)
		{
			clear();
		}
	}

	return true;
}

void CScriptBinder::net_Destroy()
{
	PROF_EVENT("CScriptBinder::net_Destroy");
	if (m_object)
	{
#ifdef ALLOW_SEH_EXCEPTIONS
		if (!g_bScriptsSEHInited)
		{
			_set_se_translator(SEH_translator_Scripts);
			g_bScriptsSEHInited = true;
		}
#endif
		try
		{
			m_object->net_Destroy();
		}
		catch (...)
		{
			clear();
		}
	}

	xr_delete(m_object);
}

void CScriptBinder::set_object(CScriptBinderObject* object)
{
	if (IsGameTypeSingleCompatible())
	{
		IVERIFY(object);
		VERIFY2(!m_object, "Cannot bind to the object twice!");
		m_object = object;
	}
	else
	{
		xr_delete(object);
	}
}

void CScriptBinder::shedule_Update(u32 time_delta)
{
	PROF_EVENT("CScriptBinder::shedule_Update");
	if (m_object)
	{
#ifdef ALLOW_SEH_EXCEPTIONS
		if (!g_bScriptsSEHInited)
		{
			_set_se_translator(SEH_translator_Scripts);
			g_bScriptsSEHInited = true;
		}
#endif
		try
		{
			m_object->shedule_Update(time_delta);
		}
		catch (...)
		{
			g_pScriptEngine->print_stack();
			clear();
		}
		
	}
}

void CScriptBinder::save			(NET_Packet &output_packet)
{
	PROF_EVENT("CScriptBinder::save")
	if (m_object) {
#ifdef ALLOW_SEH_EXCEPTIONS
		if (!g_bScriptsSEHInited)
		{
			_set_se_translator(SEH_translator_Scripts);
			g_bScriptsSEHInited = true;
		}
#endif
		try
		{
			m_object->save	(&output_packet);
		}
		catch (...) 
		{
			clear			();
		}
	}
}

void CScriptBinder::load			(IReader &input_packet)
{
	PROF_EVENT("CScriptBinder::load")
	if (m_object) {
#ifdef ALLOW_SEH_EXCEPTIONS
		if (!g_bScriptsSEHInited)
		{
			_set_se_translator(SEH_translator_Scripts);
			g_bScriptsSEHInited = true;
		}
#endif
		try
		{
			m_object->load	(&input_packet);
		}
		catch (...) 
		{
			clear			();
		}
	}
}

void CScriptBinder::Serialize(ISaveObject& Object)
{
	if (m_object) {
#ifdef ALLOW_SEH_EXCEPTIONS
		if (!g_bScriptsSEHInited)
		{
			_set_se_translator(SEH_translator_Scripts);
			g_bScriptsSEHInited = true;
		}
#endif
		try
		{
			m_object->Serialize(&Object);
		}
		catch(...) 
		{
			clear();
		}
	}
}

bool CScriptBinder::net_SaveRelevant()
{
	if (m_object) {
#ifdef ALLOW_SEH_EXCEPTIONS
		if (!g_bScriptsSEHInited)
		{
			_set_se_translator(SEH_translator_Scripts);
			g_bScriptsSEHInited = true;
		}
#endif
		try
		{
			return			(m_object->net_SaveRelevant());
		}
		catch (...) 
		{
			clear			();
		}
	}
	return							(false);
}

void CScriptBinder::net_Relcase		(CObject *object)
{
	PROF_EVENT("CScriptBinder::net_Relcase")
	CGameObject						*game_object = object->cast_game_object();
	if (m_object && game_object) {
#ifdef ALLOW_SEH_EXCEPTIONS
		if (!g_bScriptsSEHInited)
		{
			_set_se_translator(SEH_translator_Scripts);
			g_bScriptsSEHInited = true;
		}
#endif
		try
		{
			m_object->net_Relcase	(game_object->lua_game_object());
		}
		catch (...) 
		{
			clear			();
		}
	}
}
