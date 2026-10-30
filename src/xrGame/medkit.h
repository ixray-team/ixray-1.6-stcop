///////////////////////////////////////////////////////////////
// Medkit.h
// Medkit - аптечка, повышающая здоровье
///////////////////////////////////////////////////////////////

#pragma once

#include "eatable_item_object.h"
#include "../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CMedkit final : public CEatableItemObject
{
public:
	CMedkit() = default;
	virtual ~CMedkit() override = default;
};
