#pragma once
#include "inventory_item_object.h"

XREFLECT(LuaExport, Bases="CGameObject")
class AntigasFilter final : public CInventoryItemObject
{
private:
	bool bIsAllowed;
	void SetAllowed(bool flag);
public:
	HitImmunity::HitTypeSVec m_FilterProtection = {};
	HitImmunity::HitTypeSVec m_FilterDamage = {};

	XPROPERTY(LuaConstructor)
	AntigasFilter();
	virtual ~AntigasFilter() override;

	virtual void Load(const char* section) override;

	bool IsAllowed();
};
