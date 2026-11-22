#pragma once
#include "inventory_item.h"
#include "Descs/EatableItemDesc.h"
#include "../xrScripts/script_export_space.h"

class CPhysicItem;
class CEntityAlive;

XREFLECT(LuaExport)
class CEatableItem : public CInventoryItem
{
	XGENERATED_BODY()
private:
	using inherited = CInventoryItem;

protected:
	CPhysicItem* m_physic_item = nullptr;

	const SEatableItemDesc* CurrentEatableDesc = nullptr;

	u8 m_iRemainingUses = 1;
	XPROPERTY(LuaReadWrite, Name="m_bRemoveAfterUse")
	bool m_bRemoveAfterUse = true;
	XPROPERTY(LuaReadWrite, Name="m_fWeightFull")
	float m_fWeightFull = 0.0f;
	XPROPERTY(LuaReadWrite, Name="m_fWeightEmpty")
	float m_fWeightEmpty = 0.0f;

public:
	CEatableItem() = default;
	virtual ~CEatableItem() override = default;
	virtual DLL_Pure* _construct() override;

	virtual CEatableItem* cast_eatable_item() override { return this; }
	virtual CInventoryItem* cast_inventory_item() override { return this; }

	virtual void Load(const char* section) override;
	virtual void load(IReader& packet) override;
	virtual void save(NET_Packet& packet) override;
	virtual void Serialize(ISaveObject& Object) override;
	virtual bool Useful() const override;

	virtual bool net_Spawn(CSE_Abstract* DC) override;

	virtual void OnH_B_Independent(bool just_before_destroy) override;
	virtual void OnH_A_Independent() override;
	virtual	bool UseBy(CEntityAlive* npc);
	XPROPERTY(LuaCallable)
	virtual float Weight() const override;
	XPROPERTY(LuaCallable, Name="Cost")
	IC u32 script_Cost() const { return Cost(); }

	virtual void Hit(SHit* pHDS) override;
	void EatableEffects();

	XPROPERTY(LuaCallable)
	bool Empty() const { return m_iRemainingUses == 0; };
	XPROPERTY(LuaCallable)
	IC bool CanDelete() const { return m_bRemoveAfterUse == 1; };
	IC bool CanConsumeCharge() const { return EatableDesc().m_bConsumeChargeOnUse; };
	XPROPERTY(LuaCallable)
	u8 GetMaxUses() const { return EatableDesc().m_iMaxUses; };
	const SEatableItemDesc& EatableDesc() const { VERIFY(CurrentEatableDesc); return *CurrentEatableDesc; }
	XPROPERTY(LuaCallable)
	u8 GetRemainingUses() const { return m_iRemainingUses; };
	XPROPERTY(LuaCallable)
	void SetRemainingUses(u8 value)
	{
		if (value <= GetMaxUses())
		{
			m_iRemainingUses = value;
		}

		if (IsUsingCondition())
		{
			if (GetMaxUses() > 0)
			{
				SetCondition(float(m_iRemainingUses) / GetMaxUses());
			}
			else
			{
				SetCondition(0.0f);
			}
		}
	}

};