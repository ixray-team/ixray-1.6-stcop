///////////////////////////////////////////////////////////////
// Silencer.h
// Silencer - апгрейд оружия глушитель 
///////////////////////////////////////////////////////////////

#pragma once
#include "inventory_item_object.h"

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CSilencer final : public CInventoryItemObject {
private:
	typedef CInventoryItemObject inherited;
public:
	CSilencer (void);
	virtual ~CSilencer(void) override;

	virtual bool net_Spawn			(CSE_Abstract* DC) override;
	virtual void Load				(const char* section) override;
	virtual void net_Destroy		() override;

	virtual void OnH_A_Chield		() override;
	virtual void OnH_B_Independent	(bool just_before_destroy) override;

	virtual void UpdateCL			() override;
	virtual void renderable_Render	() override;

	virtual CSilencer* cast_addon_silencer() override {return this;}
};