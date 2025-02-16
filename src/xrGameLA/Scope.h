///////////////////////////////////////////////////////////////
// Scope.h
// Scope - апгрейд оружия снайперский прицел
///////////////////////////////////////////////////////////////

#pragma once

#include "inventory_item_object.h"
#include "../xrScripts/script_export_space.h"

class CScope : public CInventoryItemObject {
private:
	typedef CInventoryItemObject inherited;
public:
			CScope ();
	virtual ~CScope();
	DECLARE_SCRIPT_REGISTER_FUNCTION
};