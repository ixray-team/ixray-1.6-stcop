///////////////////////////////////////////////////////////////
// StalkerOutfit.cpp
// StalkerOutfit - защитный костюм сталкера
///////////////////////////////////////////////////////////////

#pragma once

#include "CustomOutfit.h"
#include "../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CStalkerOutfit final : public CCustomOutfit
{
public:
	CStalkerOutfit() = default;
	virtual ~CStalkerOutfit() override = default;
};