////////////////////////////////////////////////////////////////////////////
//	Module 		: danger_location.h
//	Created 	: 24.05.2004
//  Modified 	: 14.01.2005
//	Author		: Dmitriy Iassenev
//	Description : Danger location
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "memory_space.h"

class CObject;

class CDangerLocation : 
	public intrusive_base
{
public:
	u32		m_level_time;
	u32		m_interval;
	float	m_radius;
	Flags64	m_mask;

public:
	virtual bool			useful		() const;
	virtual const Fvector	&position	() const = 0;


	IC bool operator==(const Fvector& position) const
	{
		return (!!this->position().similar(position));
	}

	IC bool operator==(const CObject* object) const
	{
		return (false);
	}

	IC const Flags64& mask() const
	{
		return (m_mask);
	}
};