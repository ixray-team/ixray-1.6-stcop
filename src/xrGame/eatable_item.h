#pragma once
#include "inventory_item.h"
#include "../xrScripts/script_export_space.h"

class CPhysicItem;
class CEntityAlive;

class CEatableItem : public CInventoryItem
{
private:
	using inherited = CInventoryItem;

protected:
	CPhysicItem* m_physic_item = nullptr;

	u8 m_iMaxUses = 1;
	u8 m_iRemainingUses = 1;
	BOOL m_bRemoveAfterUse = TRUE;
	BOOL m_bConsumeChargeOnUse = FALSE;
	float m_fWeightFull = 0.0f;
	float m_fWeightEmpty = 0.0f;
	shared_str m_sUseAnimator;
	shared_str m_sLastUseAnimator;

public:
	shared_str UseText;

public:
	CEatableItem() = default;
	virtual ~CEatableItem() = default;
	virtual	DLL_Pure* _construct() override;

	virtual CEatableItem* cast_eatable_item() override { return this; }
	virtual CInventoryItem* cast_inventory_item() override { return this; }

	virtual void Load(LPCSTR section) override;
	virtual void load(IReader& packet) override;
	virtual void save(NET_Packet& packet) override;
	virtual bool Useful() const override;

	virtual BOOL net_Spawn(CSE_Abstract* DC) override;

	virtual void OnH_B_Independent(bool just_before_destroy) override;
	virtual void OnH_A_Independent() override;
	virtual	bool UseBy(CEntityAlive* npc);
	virtual float Weight() const override;

	virtual	void Hit(SHit* pHDS) override;
	void EatableEffects();

	bool Empty() const { return m_iRemainingUses == 0; };
	IC bool CanDelete() const { return m_bRemoveAfterUse == 1; };
	IC bool CanConsumeCharge() const { return m_bConsumeChargeOnUse == 1; };
	u8 GetMaxUses() const { return m_iMaxUses; };
	u8 GetRemainingUses() const { return m_iRemainingUses; };
	void SetRemainingUses(u8 value)
	{
		if (value <= m_iMaxUses)
		{
			m_iRemainingUses = value;
		}

		if (IsUsingCondition())
		{
			if (m_iMaxUses > 0)
			{
				SetCondition((float)(m_iRemainingUses / m_iMaxUses));
			}
			else
			{
				SetCondition(0.0f);
			}
		}
	}

	DECLARE_SCRIPT_REGISTER_FUNCTION
};