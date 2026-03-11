////////////////////////////////////////////////////////////////////////////
//	Module 		: script_binder.cpp
//	Created 	: 26.03.2004
//  Modified 	: 26.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Script objects binder
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
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

#define Script_ex_begin __try
#define Script_ex_end __except (ex_filter(GetExceptionCode(), GetExceptionInformation()))

#else

#define Script_ex_begin try
#define Script_ex_end catch(...)

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
	Script_ex_begin
	{
		xr_delete			(m_object);
	}
	Script_ex_end
	{
		m_object = nullptr;
	}
	init					();
}

void CScriptBinder::reinit			()
{
	if (m_object) {
		Script_ex_begin {
			m_object->reinit	();
		}
		Script_ex_end {
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

	//Script_ex_begin
	try
	{
		auto script_obj = game_object ? game_object->lua_game_object() : nullptr;
		lua_function(script_obj);
	}
	catch (...)
	//Script_ex_end
	{
		clear();
		return;
	}

	if (m_object)
	{
		try
		//Script_ex_begin
		{
			m_object->reload(section);
		}
		catch (...)
		//Script_ex_end
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
		Script_ex_begin
		{
			return m_object->net_Spawn(object);
		}
		Script_ex_end
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
		Script_ex_begin
		{
			m_object->net_Destroy();
		}
		Script_ex_end
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
		Script_ex_begin {
			m_object->shedule_Update(time_delta);
		}
		Script_ex_end {
			g_pScriptEngine->print_stack();
			clear();
		}
		
	}
}

void CScriptBinder::save			(NET_Packet &output_packet)
{
	PROF_EVENT("CScriptBinder::save")
	if (m_object) {
		Script_ex_begin {
			m_object->save	(&output_packet);
		}
		Script_ex_end {
			clear			();
		}
	}
}

void CScriptBinder::load			(IReader &input_packet)
{
	PROF_EVENT("CScriptBinder::load")
	if (m_object) {
		Script_ex_begin {
			m_object->load	(&input_packet);
		}
		Script_ex_end {
			clear			();
		}
	}
}

void CScriptBinder::Serialize(ISaveObject& Object)
{
	if (m_object) {
		Script_ex_begin {
			m_object->Serialize(&Object);
		}
		Script_ex_end {
			clear();
		}
	}
}

bool CScriptBinder::net_SaveRelevant()
{
	if (m_object) {
		Script_ex_begin {
			return			(m_object->net_SaveRelevant());
		}
		Script_ex_end {
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
		Script_ex_begin {
			m_object->net_Relcase	(game_object->lua_game_object());
		}
		Script_ex_end {
			clear			();
		}
	}
}
