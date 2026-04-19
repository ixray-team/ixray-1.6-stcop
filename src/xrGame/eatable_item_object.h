////////////////////////////////////////////////////////////////////////////
//	Module 		: eatable_item_object.h
//	Created 	: 24.03.2003
//  Modified 	: 29.01.2004
//	Author		: Yuri Dobronravin
//	Description : Eatable item object implementation
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "physic_item.h"
#include "eatable_item.h"

class CEatableItemObject :
	public CEatableItem,
	public CPhysicItem
{
public:
	CEatableItemObject() = default;
	virtual	~CEatableItemObject() = default;
	virtual DLL_Pure* _construct() override;

public:
	virtual CPhysicsShellHolder* cast_physics_shell_holder() { return this; }
	virtual CInventoryItem* cast_inventory_item() { return this; }
	virtual CAttachableItem* cast_attachable_item() { return this; }
	virtual CFoodItem* cast_food_item() { return nullptr; }
	virtual CGameObject* cast_game_object() { return this; }
	virtual CEatableItem* cast_eatable_item() { return this; }

public:
	virtual void Load(const char* section) override;
	virtual	void Hit(SHit* pHDS) override;

	virtual void OnH_B_Independent(bool just_before_destroy) override;
	virtual void OnH_A_Independent() override;
	virtual void OnH_B_Chield() override;
	virtual void OnH_A_Chield() override;
	virtual void UpdateCL() override;
	virtual void OnEvent(NET_Packet& P, u16 type) override;
	virtual bool net_Spawn(CSE_Abstract* DC) override;
	virtual void net_Destroy() override;
	virtual void net_Import(NET_Packet& P) override;					// import from server
	virtual void net_Export(NET_Packet& P) override;					// export to server
	virtual void save(NET_Packet& output_packet) override;
	virtual void load(IReader& input_packet) override;
	virtual bool net_SaveRelevant() override { return true; }
	virtual void renderable_Render() override;
	virtual void reload(const char* section) override;
	virtual void reinit() override;
	virtual void activate_physic_shell() override;
	virtual void on_activate_physic_shell() override;
public:
	////////// network //////////////////////////////////////////////////
	virtual void make_Interpolation() override;
	virtual void PH_B_CrPr() override; // actions & operations before physic correction-prediction steps
	virtual void PH_I_CrPr() override; // actions & operations after correction before prediction steps
#ifdef DEBUG
	virtual void PH_Ch_CrPr();
#endif
	virtual void PH_A_CrPr() override; // actions & operations after phisic correction-prediction steps
	virtual bool NeedToDestroyObject() const override;

protected:
#ifdef DEBUG_DRAW
	virtual void OnRender() override;
#endif

public:
	virtual bool Useful() const override;

public:
	virtual u32	ef_weapon_type() const override;
protected:
	virtual bool use_parent_ai_locations() const override
	{
		return CAttachableItem::use_parent_ai_locations();
	}
};
