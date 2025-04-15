#pragma once
#include "inventory_item.h"
#include "../xrScripts/script_export_space.h"

class CPhysicItem;
class CEntityAlive;

class CEatableItem : public CInventoryItem {
private:
	typedef CInventoryItem	inherited;

protected:
	CPhysicItem* m_physic_item;

	u8 m_iMaxUses;
	float m_iPortionsMarker;
	BOOL m_bRemoveAfterUse;
	BOOL m_bConsumeChargeOnUse;
	float m_fWeightFull;
	float m_fWeightEmpty;
	bool bUseHUDAnim = false;

public:
	shared_str UseText;
	float m_eat_condition = 1;

public:
	CEatableItem();
	virtual ~CEatableItem();
	virtual	DLL_Pure* _construct();

	virtual CEatableItem* cast_eatable_item() { return this; }

	virtual void			Load(LPCSTR section);
	virtual void			load(IReader& packet);
	virtual void			save(NET_Packet& packet);
	virtual bool			Useful() const;

	virtual BOOL			net_Spawn(CSE_Abstract* DC);

	virtual void			OnH_B_Independent(bool just_before_destroy);
	virtual void			OnH_A_Independent();
	virtual	bool			UseBy(CEntityAlive* npc);
	virtual float			Weight() const;

	IC bool Empty() const { return GetRemainingUses() == 0; };
	IC bool CanDelete() const { return m_bRemoveAfterUse == 1; };
	IC bool CanConsumeCharge() const { return m_bConsumeChargeOnUse == 1; };
	IC u8 GetMaxUses() { return m_iMaxUses; };
	IC u8 GetRemainingUses() const { return (u8)roundf(((float)m_iMaxUses) * m_fCondition); };
	IC void SetRemainingUses(u8 value) { m_fCondition = ((float)value / (float)m_iMaxUses); clamp(m_fCondition, 0.f, 1.f); };

	DECLARE_SCRIPT_REGISTER_FUNCTION
};