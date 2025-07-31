////////////////////////////////////////////////////////////////////////////
//	Module 		: member_enemy.h
//	Created 	: 24.05.2004
//  Modified 	: 14.01.2005
//	Author		: Dmitriy Iassenev
//	Description : Member enemy
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "memory_space.h"

class CEntityAlive;

class CMemberEnemy
{

public:
	const CEntityAlive	*m_object;
	Flags64				m_mask;
	Flags64				m_distribute_mask;
	float				m_probability;
	Fvector				m_enemy_position;
	u32					m_level_time;

public:
	IC					CMemberEnemy		(const CEntityAlive *object, u64 mask);
	IC		bool		operator==			(const CEntityAlive *object) const;
	IC		bool		operator<			(const CMemberEnemy &enemy) const;
};

#include "member_enemy_inline.h"