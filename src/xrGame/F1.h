#pragma once

#include "grenade.h"
#include "../xrScripts/script_export_space.h"

class CF1 :
	public CGrenade
{
	typedef CGrenade inherited;
public:
	CF1(void);
	virtual ~CF1(void);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};