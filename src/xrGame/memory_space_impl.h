////////////////////////////////////////////////////////////////////////////
//	Module 		: memory_space_impl.h
//	Created 	: 25.05.2004
//  Modified 	: 25.05.2004
//	Author		: Dmitriy Iassenev
//	Description : Memory space implementation
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "memory_space.h"
#include "GameObject.h"
#include "Level.h"
#include "ai_space.h"
#include "ai_object_location.h"
#include "level_graph.h"

IC void CObjectParams::fill(const CGameObject* game_object)
{
	m_level_vertex_id = game_object ? game_object->ai_location().level_vertex_id() : u32(-1);

	if (game_object)
	{
		game_object->Center(m_position);
		m_position.set(game_object->Position().x, m_position.y, game_object->Position().z);
		return;
	}

	m_position = Fvector().set(0.f, 0.f, 0.f);
}

IC CMemoryObject::CMemoryObject()
{
	m_squad_mask.one();
	m_object = 0;
}

IC void CMemoryObject::fill(const CGameObject* game_object, const CGameObject* self, const u64& mask)
{
	m_last_level_time = m_level_time;
	m_level_time = Device.dwTimeGlobal;

	m_object = game_object;
	m_object_params.fill(game_object);
	m_self_params.fill(self);
	m_squad_mask.assign(mask);
	SMemoryObject::fill();
}