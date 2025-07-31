#pragma once

#include "eatable_item_object.h"
#include "../xrScripts/script_export_space.h"

class CFoodItem: public CEatableItemObject
{
public:
						CFoodItem			();
	virtual				~CFoodItem			();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
