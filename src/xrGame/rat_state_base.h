////////////////////////////////////////////////////////////////////////////
//	Module 		: rat_state_base.h
//	Created 	: 31.08.2007
//  Modified 	: 31.08.2007
//	Author		: Dmitriy Iassenev
//	Description : rat state base class
////////////////////////////////////////////////////////////////////////////
#pragma once

class CAI_Rat;

class rat_state_base
{
private:
	CAI_Rat	*m_object;

public:
	rat_state_base(const rat_state_base& other) = delete;
	virtual ~rat_state_base() = default;

	rat_state_base& operator=(const rat_state_base& other) = delete;

	IC				rat_state_base	();
			void	construct		(CAI_Rat *object);
	virtual	void	initialize		() = 0;
	virtual	void	execute			() = 0;
	virtual	void	finalize		() = 0;
	IC		CAI_Rat &object			() const;
};

#include "rat_state_base_inline.h"