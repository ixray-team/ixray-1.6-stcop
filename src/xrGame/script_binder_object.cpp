////////////////////////////////////////////////////////////////////////////
//	Module 		: script_binder_object.cpp
//	Created 	: 29.03.2004
//  Modified 	: 29.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Script object binder
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "script_binder_object.h"
#include "script_game_object.h"

CScriptBinderObject::CScriptBinderObject(luabind::object object)
{
	if (!I_ASSERT_M(object.is_valid() && object.type() != LUA_TNIL, "\"self\" argument in object_binder is invalid!"))
	{
		ai().script_engine().print_stack();
		return;
	}
	auto Obj = luabind::object_cast_nothrow<CScriptGameObject*>(object).value_or(nullptr);
	if (!I_ASSERT(Obj))
	{
		Msg("Type is [%d]", object.type());
	}
	m_object = Obj;
}

CScriptBinderObject::~CScriptBinderObject	()
{
#ifdef DEBUG
	if (m_object)
		Msg			("Destroying binded object %s",m_object->Name());
#endif
}

void CScriptBinderObject::FinishInitialization()
{
}

void CScriptBinderObject::reinit			()
{
}

void CScriptBinderObject::reload			(const char* section)
{
}

bool CScriptBinderObject::net_Spawn			(SpawnType DC)
{
	return			(true);
}

void CScriptBinderObject::net_Destroy		()
{
}

void CScriptBinderObject::net_Import		(NET_Packet *net_packet)
{
}

void CScriptBinderObject::net_Export		(NET_Packet *net_packet)
{
}

void CScriptBinderObject::shedule_Update	(u32 time_delta)
{
}

void CScriptBinderObject::save				(NET_Packet *output_packet)
{
}

void CScriptBinderObject::load				(IReader	*input_packet)
{
}

void CScriptBinderObject::Serialize(ISaveObject* Object)
{
}

bool CScriptBinderObject::net_SaveRelevant	()
{
	return		(false);
}

void CScriptBinderObject::net_Relcase		(CScriptGameObject *object)
{
}
