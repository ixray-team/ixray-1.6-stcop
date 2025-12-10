////////////////////////////////////////////////////////////////////////////
//	Module 		: inventory_item_object.h
//	Created 	: 24.03.2003
//  Modified 	: 27.12.2004
//	Author		: Victor Reutsky, Yuri Dobronravin
//	Description : Inventory item object implementation
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "physic_item.h"
#include "inventory_item.h"

class CFlamethrower;
class CCustomDetector;
class CCustomDevice;
class CWeaponBinoculars;
class CWeaponKnife;
class CWeaponMagazined;
class CWeaponMagazinedWGrenade;
class CWeaponBM16;
class CWeapon;
class CWeaponRPG7;
class CWeaponRG6;
class CHudItem;
class CTorch;
class CBolt;
class CPda;
class CGrenade;
class CMissile;
class CSilencer;
class CScope;
class CGrenadeLauncher;
class CArmorBase;

class CInventoryItemObject : public CInventoryItem, public CPhysicItem
{
	shared_str m_sNewVisualName;
public:
	CInventoryItemObject() = default;
	virtual	~CInventoryItemObject() = default;
	virtual DLL_Pure* _construct();

public:
	virtual CPhysicsShellHolder* cast_physics_shell_holder() override { return this; }
	virtual CInventoryItem* cast_inventory_item() override { return this; }
	virtual CAttachableItem* cast_attachable_item() override { return this; }
	virtual CGameObject* cast_game_object() override { return this; }
	virtual CHudItem* cast_hud_item() override { return nullptr; }
	virtual CCustomDetector* cast_custom_detector() override { return nullptr; }
	virtual CCustomDevice* cast_custom_device() override { return nullptr; }
	virtual CWeaponBinoculars* cast_weapon_binoculars() override { return nullptr; }
	virtual CWeaponKnife* cast_weapon_knife() override { return nullptr; }
	virtual CWeaponMagazined* cast_weapon_magazined() override { return nullptr; }
	virtual CWeaponMagazinedWGrenade* cast_weapon_magazined_w_grenade() override { return nullptr; }
	virtual CWeaponBM16* cast_weapon_bm16() override { return nullptr; }
	virtual CWeapon* cast_weapon() override { return nullptr; }
	virtual CTorch* cast_torch() override { return nullptr; }
	virtual CWeaponRPG7* cast_weapon_rpg7() override { return nullptr; }
	virtual CWeaponRG6* cast_weapon_rg6() override { return nullptr; }
	virtual CBolt* cast_bolt() override { return nullptr; }
	virtual CPda* cast_pda() override { return nullptr; }
	virtual CGrenade* cast_grenade() override { return nullptr; }
	virtual CMissile* cast_missile() override { return nullptr; }
	virtual CSilencer* cast_addon_silencer() override { return nullptr; }
	virtual CScope* cast_addon_scope() override { return nullptr; }
	virtual CGrenadeLauncher* cast_addon_grenade_launcher() override { return nullptr; }
	virtual CPhysicItem* cast_physics_item() override { return this; }
	virtual CWeaponShotgun* cast_weapon_shotgun() override { return nullptr; }
	virtual CArmorBase* cast_armorbase() override { return nullptr; }
	virtual CFlamethrower* cast_flamethrower() override { return nullptr; }

public:
	virtual void Load(const char* section);
	virtual	void Hit(SHit* pHDS);

	virtual void OnH_B_Independent(bool just_before_destroy);
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
	virtual void Serialize(ISaveObject& Object) override;
	virtual bool net_SaveRelevant() override { return true; }
	virtual void renderable_Render() override;
	virtual void reload(const char* section) override;
	virtual void reinit() override;
	virtual void activate_physic_shell() override;
	virtual void on_activate_physic_shell() override;
	virtual	void modify_holder_params(float& range, float& fov) const override;

	virtual void OnChangeVisual() override;

	virtual bool install_upgrade_impl(const char* section, bool test) override;

public:
	////////// network //////////////////////////////////////////////////
	virtual void make_Interpolation();
	virtual void PH_B_CrPr(); // actions & operations before physic correction-prediction steps
	virtual void PH_I_CrPr(); // actions & operations after correction before prediction steps
#ifdef DEBUG
	virtual void PH_Ch_CrPr(); // 
#endif
	virtual void PH_A_CrPr(); // actions & operations after phisic correction-prediction steps
	virtual bool NeedToDestroyObject() const;

protected:
#ifdef DEBUG_DRAW
	virtual void OnRender();
#endif

public:
	virtual bool Useful() const;

public:
	virtual u32	ef_weapon_type() const;
protected:
	virtual bool use_parent_ai_locations() const
	{
		return CAttachableItem::use_parent_ai_locations();
	}

};