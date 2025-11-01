#pragma once

#include "inventory_item_object.h"
#include "../xrScripts/script_export_space.h"

class AntigasFilter final : public CInventoryItemObject
{
private:
	bool bIsAllowed;
	void SetAllowed(bool flag);
public:
	HitImmunity::HitTypeSVec m_FilterProtection = {};
	HitImmunity::HitTypeSVec m_FilterDamage = {};

	AntigasFilter();
	virtual	~AntigasFilter();

	virtual void Load(LPCSTR section) override;

	bool IsAllowed();

	DECLARE_SCRIPT_REGISTER_FUNCTION
};
