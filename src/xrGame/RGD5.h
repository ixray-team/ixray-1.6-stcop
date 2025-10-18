#pragma once

#include "Grenade.h"
#include "../xrScripts/script_export_space.h"

class CRGD5 final : public CGrenade
{
	using inherited = CGrenade;
public:
	CRGD5() = default;
	virtual ~CRGD5() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};