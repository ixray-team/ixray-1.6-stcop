#pragma once

#include "Grenade.h"
#include "../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CF1 final : public CGrenade
{
	using inherited = CGrenade;
public:
	CF1() = default;
	virtual ~CF1() override = default;
};