///////////////////////////////////////////////////////////////
// GrenadeLauncher.h
// GrenadeLauncher - апгрейд оружия поствольный гранатомет
///////////////////////////////////////////////////////////////

#pragma once
#include "inventory_item_object.h"

class CGrenadeLauncher final : public CInventoryItemObject {
private:
	typedef CInventoryItemObject inherited;
public:
	CGrenadeLauncher (void);
	virtual ~CGrenadeLauncher(void) override;

	virtual bool net_Spawn			(CSE_Abstract* DC) override;
	virtual void Load				(const char* section) override;
	virtual void net_Destroy		() override;

	virtual void OnH_A_Chield		() override;
	virtual void OnH_B_Independent	(bool just_before_destroy) override;

	virtual void UpdateCL			() override;
	virtual void renderable_Render	() override;

	float	GetGrenadeVel() {return m_fGrenadeVel;}

	virtual CGrenadeLauncher* cast_addon_grenade_launcher() override {return this;}
	
protected:
	//стартовая скорость вылета подствольной гранаты
	float m_fGrenadeVel;
};