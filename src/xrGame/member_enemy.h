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

class CMemberEnemy {
public:
	typedef u64	u64;
	typedef _flags<u64>			mask_type;

public:
	const CEntityAlive	*m_object;
	mask_type			m_mask;
	mask_type			m_distribute_mask;
	float				m_probability;
	Fvector				m_enemy_position;
	u32					m_level_time;

public:
	IC					CMemberEnemy		(const CEntityAlive *object, u64 mask);
	IC		bool		operator==			(const CEntityAlive *object) const;
	IC		bool		operator<			(const CMemberEnemy &enemy) const;
};

#include "member_enemy_inline.h"