#pragma once

#include "eatable_item_object.h"
#include "../xrScripts/script_export_space.h"

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CFoodItem : public CEatableItemObject
{
public:
	CFoodItem() = default;
	virtual ~CFoodItem() override = default;

	virtual CFoodItem* cast_food_item() override { return this; }
};
