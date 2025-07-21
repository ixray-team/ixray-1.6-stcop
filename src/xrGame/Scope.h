///////////////////////////////////////////////////////////////
// Scope.h
// Scope - апгрейд оружия снайперский прицел
///////////////////////////////////////////////////////////////

#pragma once

#include "inventory_item_object.h"
#include "../xrScripts/script_export_space.h"

class CScope : 
	public CInventoryItemObject 
{
private:
	typedef CInventoryItemObject inherited;
public:
			CScope ();
	virtual ~CScope();
	
	virtual CScope* cast_addon_scope() {return this;}
	
	DECLARE_SCRIPT_REGISTER_FUNCTION
};