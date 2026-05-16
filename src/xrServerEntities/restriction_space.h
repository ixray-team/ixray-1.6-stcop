////////////////////////////////////////////////////////////////////////////
//	Module 		: restriction_space.h
//	Created 	: 30.08.2004
//  Modified 	: 30.08.2004
//	Author		: Dmitriy Iassenev
//	Description : Restriction space
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../xrEngine/device.h"
#include "../xrEngine/AI/restrictor_types.h"

namespace RestrictionSpace {
	struct CTimeIntrusiveBase : public intrusive_base {
		u32			m_last_time_dec;

		IC			CTimeIntrusiveBase	() : m_last_time_dec(0)
		{
		}

		template <typename T>
		IC	void	_release		(T*object)
		{
			m_last_time_dec = Device.dwTimeGlobal;
		}
	};
};

