////////////////////////////////////////////////////////////////////////////
//	Module 		: danger_object_location_inline.h
//	Created 	: 24.05.2004
//  Modified 	: 14.01.2005
//	Author		: Dmitriy Iassenev
//	Description : Danger object location inline functions
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "GameObject.h"

IC	CDangerObjectLocation::CDangerObjectLocation	(const CGameObject *object, u32 level_time, u32 interval, float radius, const u64 &mask)
{
	VERIFY			(object);
	m_object		= object;
	m_object_id		= object->ID();
	m_cached_position = object->Position();
	m_level_time	= level_time;
	m_interval		= interval;
	m_radius		= radius;
	m_mask.assign	(mask);
}
