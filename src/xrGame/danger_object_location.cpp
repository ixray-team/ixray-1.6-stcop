////////////////////////////////////////////////////////////////////////////
//	Module 		: danger_object_location.cpp
//	Created 	: 24.05.2004
//  Modified 	: 14.01.2005
//	Author		: Dmitriy Iassenev
//	Description : Danger object location
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "danger_object_location.h"
#include "GameObject.h"

static bool IsObjectValid(const CGameObject* object, u16 object_id)
{
	if (object == nullptr)
	{
		return false;
	}

	return g_pGameLevel->Objects.net_Find(object_id) == object;
}

const Fvector& CDangerObjectLocation::position() const
{
	if (IsObjectValid(m_object, m_object_id))
	{
		m_cached_position = m_object->Position();
	}

	return (m_cached_position);
}

bool CDangerObjectLocation::useful() const
{
	if (IsObjectValid(m_object, m_object_id))
	{
		return true;
	}

	return (CDangerLocation::useful());
}

bool CDangerObjectLocation::operator==(const CObject* object) const
{
	if (object == nullptr)
	{
		return false;
	}

	if (m_object == object)
	{
		return true;
	}

	return (m_object_id == object->ID());
}