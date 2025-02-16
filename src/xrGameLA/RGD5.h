#pragma once

#include "grenade.h"
#include "../xrScripts/script_export_space.h"

class CRGD5 :
	public CGrenade
{
	typedef CGrenade inherited;
public:
	CRGD5(void);
	virtual ~CRGD5(void);

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
