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
	try {
		xr_delete			(m_object);
	}
	catch(...) {
		m_object			= 0;
	}
	init					();
}

void CScriptBinder::reinit			()
{
	if (m_object) {
		try {
			m_object->reinit	();
		}
		catch(...) {
			clear			();
		}
	}
}

void CScriptBinder::Load			(LPCSTR section)
{
}

void CScriptBinder::reload(LPCSTR section)
{
	PROF_EVENT("CScriptBinder::reload");

	VERIFY(!m_object);
	if (!pSettings->line_exist(section, "script_binding"))
		return;

	luabind::functor<void>	lua_function;
	if (!ai().script_engine().functor(pSettings->r_string(section, "script_binding"), lua_function))
	{
		ai().script_engine().script_log(ScriptStorage::eLuaMessageTypeError, "function %s is not loaded!", pSettings->r_string(section, "script_binding"));
		return;
	}

	CGameObject* game_object = smart_cast<CGameObject*>(this);

	try
	{
		lua_function(game_object ? game_object->lua_game_object() : 0);
	}
	catch (...)
	{
		clear();
		return;
	}

	if (m_object)
	{
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

BOOL CScriptBinder::net_Spawn(CSE_Abstract* DC)
{
	PROF_EVENT("CScriptBinder::net_Spawn");
	CSE_Abstract* abstract = (CSE_Abstract*)DC;
	CSE_ALifeObject* object = smart_cast<CSE_ALifeObject*>(abstract);

	if (object && m_object)
	{
		try
		{
			return m_object->net_Spawn(object);
		}
		catch (...)
		{
			clear();
		}
	}

	return TRUE;
}

void CScriptBinder::net_Destroy()
{
	PROF_EVENT("CScriptBinder::net_Destroy");
	if (m_object)
	{
		try
		{
			m_object->net_Destroy();
		}
		catch(...)
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
		m_object->shedule_Update(time_delta);
	}
}

void CScriptBinder::save			(NET_Packet &output_packet)
{
	PROF_EVENT("CScriptBinder::save")
	if (m_object) {
		try {
			m_object->save	(&output_packet);
		}
		catch(...) {
			clear			();
		}
	}
}

void CScriptBinder::load			(IReader &input_packet)
{
	PROF_EVENT("CScriptBinder::load")
	if (m_object) {
		try {
			m_object->load	(&input_packet);
		}
		catch(...) {
			clear			();
		}
	}
}

BOOL CScriptBinder::net_SaveRelevant()
{
	if (m_object) {
		try {
			return			(m_object->net_SaveRelevant());
		}
		catch(...) {
			clear			();
		}
	}
	return							(FALSE);
}

void CScriptBinder::net_Relcase		(CObject *object)
{
	PROF_EVENT("CScriptBinder::net_Relcase")
	CGameObject						*game_object = object->cast_game_object();
	if (m_object && game_object) {
		try {
			m_object->net_Relcase	(game_object->lua_game_object());
		}
		catch(...) {
			clear			();
		}
	}
}
