////////////////////////////////////////////////////////////////////////////
//	Module 		: script_binder_object_wrapper.cpp
//	Created 	: 29.03.2004
//  Modified 	: 29.03.2004
//	Author		: Dmitriy Iassenev
//	Description : Script object binder wrapper
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "script_binder_object_wrapper.h"
#include "script_game_object.h"
#include "xrServer_Objects_ALife.h"
#include "luabind/class_info.hpp"

enum class ScriptBinderMethods
{
	reinit,
	reload,
	net_spawn,
	net_destroy,
	net_import,
	net_export,
	update,
	save,
	load,
	Serialize,
	net_save_relevant,
	net_Relcase,
};

xr_hash_map<shared_str, bool[int(ScriptBinderMethods::net_Relcase)+1]> ObjectsReflectionData;

CScriptBinderObjectWrapper::CScriptBinderObjectWrapper(luabind::object object) :
	CScriptBinderObject	(object)
{}

void CScriptBinderObjectWrapper::FinishInitialization()
{
	auto L = ai().script_engine().lua();
	I_ASSERT(lua_type(L, -1) == LUA_TUSERDATA);
	auto obj_rep = static_cast<luabind::detail::object_rep*>(lua_touserdata(L, -1));
	I_ASSERT(obj_rep);
	auto class_rep = obj_rep->crep();
	I_ASSERT(class_rep);
	shared_str ClassName = class_rep->name();
	
	if (ObjectsReflectionData.contains(ClassName))
	{
		ReflectionData = ObjectsReflectionData[ClassName];
		return;
	}
	ReflectionData = ObjectsReflectionData[ClassName];
	
	for (int i = (int)ScriptBinderMethods::reinit; i <= (int)ScriptBinderMethods::net_Relcase; ++i)
	{
		lua_getfield(L, -1, magic_enum::enum_name<ScriptBinderMethods>((ScriptBinderMethods)i).data());
		ReflectionData[i] = lua_isfunction(L, -1);
		lua_pop(L, 1);
		//auto elem = self[magic_enum::enum_name<ScriptBinderMethods>((ScriptBinderMethods)i).data()];
		//ReflectionData[i] = elem && elem.type() == LUA_TFUNCTION;
	}
}

CScriptBinderObjectWrapper::~CScriptBinderObjectWrapper ()
{
}

void CScriptBinderObjectWrapper::reinit					()
{
	if (ReflectionData[int(ScriptBinderMethods::reinit)])
	{
		luabind::call_member<void>(this, "reinit");
	}
}

void CScriptBinderObjectWrapper::reinit_static			(CScriptBinderObject *script_binder_object)
{
	script_binder_object->CScriptBinderObject::reinit	();
}

void CScriptBinderObjectWrapper::reload					(const char* section)
{
	if (ReflectionData[int(ScriptBinderMethods::reload)])
	{
		luabind::call_member<void>(this, "reload", section);
	}
}

void CScriptBinderObjectWrapper::reload_static			(CScriptBinderObject *script_binder_object, const char* section)
{
	script_binder_object->CScriptBinderObject::reload	(section);
}

bool CScriptBinderObjectWrapper::net_Spawn				(SpawnType DC)
{
	if (ReflectionData[int(ScriptBinderMethods::net_spawn)])
	{
		return luabind::call_member<bool>(this, "net_spawn", DC);
	}
	return true;
}

bool CScriptBinderObjectWrapper::net_Spawn_static		(CScriptBinderObject *script_binder_object, SpawnType DC)
{
	return							(script_binder_object->CScriptBinderObject::net_Spawn(DC));
}

void CScriptBinderObjectWrapper::net_Destroy			()
{
	if (ReflectionData[int(ScriptBinderMethods::net_destroy)])
	{
		luabind::call_member<void>(this, "net_destroy");
	}
}

void CScriptBinderObjectWrapper::net_Destroy_static		(CScriptBinderObject *script_binder_object)
{
	script_binder_object->CScriptBinderObject::net_Destroy();
}

void CScriptBinderObjectWrapper::net_Import				(NET_Packet *net_packet)
{
	if (ReflectionData[int(ScriptBinderMethods::net_import)])
	{
		luabind::call_member<void>(this, "net_import", net_packet);
	}
}

void CScriptBinderObjectWrapper::net_Import_static		(CScriptBinderObject *script_binder_object, NET_Packet *net_packet)
{
	script_binder_object->CScriptBinderObject::net_Import	(net_packet);
}

void CScriptBinderObjectWrapper::net_Export				(NET_Packet *net_packet)
{
	if (ReflectionData[int(ScriptBinderMethods::net_export)])
	{
		luabind::call_member<void>(this, "net_export", net_packet);
	}
}

void CScriptBinderObjectWrapper::net_Export_static		(CScriptBinderObject *script_binder_object, NET_Packet *net_packet)
{
	script_binder_object->CScriptBinderObject::net_Export	(net_packet);
}

void CScriptBinderObjectWrapper::shedule_Update			(u32 time_delta)
{
	if (ReflectionData[int(ScriptBinderMethods::update)])
	{
		luabind::call_member<void>(this, "update", time_delta);
	}
}

void CScriptBinderObjectWrapper::shedule_Update_static	(CScriptBinderObject *script_binder_object, u32 time_delta)
{
	script_binder_object->CScriptBinderObject::shedule_Update	(time_delta);
}

void CScriptBinderObjectWrapper::save					(NET_Packet *output_packet)
{
	if (ReflectionData[int(ScriptBinderMethods::save)])
	{
		luabind::call_member<void>(this, "save", output_packet);
	}
}

void CScriptBinderObjectWrapper::save_static			(CScriptBinderObject *script_binder_object, NET_Packet *output_packet)
{
	script_binder_object->CScriptBinderObject::save		(output_packet);
}

void CScriptBinderObjectWrapper::load					(IReader *input_packet)
{
	if (ReflectionData[int(ScriptBinderMethods::load)])
	{
		luabind::call_member<void>(this, "load", input_packet);
	}
}

void CScriptBinderObjectWrapper::load_static			(CScriptBinderObject *script_binder_object, IReader *input_packet)
{
	script_binder_object->CScriptBinderObject::load		(input_packet);
}

void CScriptBinderObjectWrapper::Serialize(ISaveObject* Object)
{
	if (ReflectionData[int(ScriptBinderMethods::Serialize)])
	{
		luabind::call_member<void>(this, "Serialize", Object);
	}
}

void CScriptBinderObjectWrapper::Serialize_static(CScriptBinderObject* script_binder_object, ISaveObject* Object)
{
	script_binder_object->CScriptBinderObject::Serialize(Object);
}

bool CScriptBinderObjectWrapper::net_SaveRelevant		()
{
	if (ReflectionData[int(ScriptBinderMethods::net_save_relevant)])
	{
		return luabind::call_member<bool>(this, "net_save_relevant");
	}
	return true;
}

bool CScriptBinderObjectWrapper::net_SaveRelevant_static(CScriptBinderObject *script_binder_object)
{
	return							(script_binder_object->CScriptBinderObject::net_SaveRelevant());
}

void CScriptBinderObjectWrapper::net_Relcase			(CScriptGameObject *object)
{
	if (ReflectionData[int(ScriptBinderMethods::net_Relcase)])
	{
		luabind::call_member<void>(this,"net_Relcase",object);
	}
}

void CScriptBinderObjectWrapper::net_Relcase_static		(CScriptBinderObject *script_binder_object, CScriptGameObject *object)
{
	script_binder_object->CScriptBinderObject::net_Relcase	(object);
}