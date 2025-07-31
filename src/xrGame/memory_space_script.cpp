////////////////////////////////////////////////////////////////////////////
//	Module 		: memory_space_script.cpp
//	Created 	: 25.12.2003
//  Modified 	: 25.12.2003
//	Author		: Dmitriy Iassenev
//	Description : Memory space script export
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "memory_space.h"
#include "script_game_object.h"
#include "GameObject.h"
#include "entity_alive.h"
#include "danger_object.h"

using namespace luabind;


u16 CMemoryObject::object_id(const CObject* object)
{
	return (object ? u16(object->ID()) : u16(0xffff));
}

bool CMemoryObject::operator==(u16 id) const
{
	return (object_id(m_object) == id);
}

CScriptGameObject *not_yet_visible_object(const MemorySpace::CNotYetVisibleObject &object)
{
	return			(object.m_object->lua_game_object());
}

int get_sound_type(const CSoundObject &sound_object)
{
	return			((int)sound_object.m_sound_type);
}

CScriptGameObject *get_memory_object(const MemorySpace::CMemoryObject &memory_object)
{
	return (memory_object.m_object->lua_game_object());
}

CScriptGameObject *CDangerObject_object(const CDangerObject *self_)
{
	VERIFY			(self_);
	return			(self_->object() ? self_->object()->lua_game_object() : 0);
}

CScriptGameObject *CDangerObject_dependent_object(const CDangerObject *self_)
{
	VERIFY				(self_);
	if (!self_->dependent_object())
		return			(0);

	const CGameObject	*game_object = smart_cast<const CGameObject*>(self_->dependent_object());
	return				(game_object ? game_object->lua_game_object() : 0);
}

Fvector CDangerObject__position	(const CDangerObject *self_)
{
	THROW				(self_);
	return				(self_->position());
}

#pragma optimize("s",on)
void CMemoryInfo::script_register(lua_State *L)
{
	module(L)
	[
		class_<SRotation>("rotation")
			.def_readwrite("yaw",			&SRotation::yaw)
			.def_readwrite("pitch",			&SRotation::pitch),
			
		class_<MemorySpace::SObjectParams>("object_params")
			.def_readonly("level_vertex",	&MemorySpace::SObjectParams::m_level_vertex_id)
			.def_readonly("position",		&MemorySpace::SObjectParams::m_position),
			
		class_<MemorySpace::SMemoryObject>("memory_object")
			.def_readonly("level_time",		&MemorySpace::SMemoryObject::m_level_time)
			.def_readonly("last_level_time",&MemorySpace::SMemoryObject::m_last_level_time)
			,

		class_<MemorySpace::CMemoryObject,MemorySpace::SMemoryObject>("game_memory_object")
			.def_readonly("object_info",	&MemorySpace::CMemoryObject::m_object_params)
			.def_readonly("self_info",		&MemorySpace::CMemoryObject::m_self_params)
			.def("object",					&get_memory_object),

		class_<MemorySpace::CHitObject,MemorySpace::CMemoryObject>("hit_memory_object")
			.def_readonly("direction",		&MemorySpace::CHitObject::m_direction)
			.def_readonly("bone_index",		&MemorySpace::CHitObject::m_bone_index)
			.def_readonly("amount",			&MemorySpace::CHitObject::m_amount),
		
		class_<MemorySpace::CVisibleObject,MemorySpace::CMemoryObject>("visible_memory_object")
			,

		class_<MemorySpace::CMemoryInfo,MemorySpace::CVisibleObject>("memory_info")
			.def_readonly("visual_info",	&MemorySpace::CMemoryInfo::m_visual_info)
			.def_readonly("sound_info",		&MemorySpace::CMemoryInfo::m_sound_info)
			.def_readonly("hit_info",		&MemorySpace::CMemoryInfo::m_hit_info),

		class_<MemorySpace::CSoundObject,MemorySpace::CMemoryObject>("sound_memory_object")
			.def("type",					&MemorySpace::CSoundObject::sound_type)
			.def_readonly("power",			&MemorySpace::CSoundObject::m_power),

		class_<MemorySpace::CNotYetVisibleObject>("not_yet_visible_object")
			.def_readonly("value",			&MemorySpace::CNotYetVisibleObject::m_value)
			.def("object",					&not_yet_visible_object),

		class_<CDangerObject>("danger_object")
			.enum_("danger_type")
			[
				value("bullet_ricochet",	CDangerObject::eDangerTypeBulletRicochet),
				value("attack_sound",		CDangerObject::eDangerTypeAttackSound),
				value("entity_attacked",	CDangerObject::eDangerTypeEntityAttacked),
				value("entity_death",		CDangerObject::eDangerTypeEntityDeath),
				value("entity_corpse",		CDangerObject::eDangerTypeFreshEntityCorpse),
				value("attacked",			CDangerObject::eDangerTypeAttacked),
				value("grenade",			CDangerObject::eDangerTypeGrenade),
				value("enemy_sound",		CDangerObject::eDangerTypeEnemySound)
			]
			.enum_("danger_perceive_type")
			[
				value("visual",				CDangerObject::eDangerPerceiveTypeVisual),
				value("sound",				CDangerObject::eDangerPerceiveTypeSound),
				value("hit",				CDangerObject::eDangerPerceiveTypeHit)
			]
			.def(							const_self == other<CDangerObject>())
			.def("position",				&CDangerObject__position)
			.def("time",					&CDangerObject::time)
			.def("type",					&CDangerObject::type)
			.def("perceive_type",			&CDangerObject::perceive_type)
			.def("object",					&CDangerObject_object)
			.def("dependent_object",		&CDangerObject_dependent_object)
	];
}