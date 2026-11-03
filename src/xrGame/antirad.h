///////////////////////////////////////////////////////////////
// Antirad.h
// Antirad - таблетки выводящие радиацию
///////////////////////////////////////////////////////////////

#pragma once

#include "eatable_item_object.h"
#include "../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CAntirad final : public CEatableItemObject
{
public:
	CAntirad() = default;
	virtual ~CAntirad() override = default;
};
