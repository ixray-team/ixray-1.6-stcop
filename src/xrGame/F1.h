#pragma once

#include "Grenade.h"
#include "../xrScripts/script_export_space.h"

class CF1 final : public CGrenade
{
	using inherited = CGrenade;
public:
	CF1() = default;
	virtual ~CF1() = default;

	DECLARE_SCRIPT_REGISTER_FUNCTION
};