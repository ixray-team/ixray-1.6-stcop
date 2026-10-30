#pragma once

#include "Grenade.h"
#include "../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CRGD5 final : public CGrenade
{
	using inherited = CGrenade;
public:
	CRGD5() = default;
	virtual ~CRGD5() override = default;
};