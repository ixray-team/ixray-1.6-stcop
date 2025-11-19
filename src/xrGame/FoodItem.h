#pragma once

#include "eatable_item_object.h"
#include "../xrScripts/script_export_space.h"

class CFoodItem : public CEatableItemObject
{
public:
	CFoodItem() = default;
	virtual ~CFoodItem() = default;

	virtual CFoodItem* cast_food_item() override { return this; }

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
