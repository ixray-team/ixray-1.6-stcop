#pragma once

#include "inventory_item_object.h"
#include "HudItem.h"

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
class CMissile;
class CBolt;
class CGrenade;
class CPhysicItem;
class CPhysicsShellHolder;

class CHudItemObject : public CInventoryItemObject,
	public CHudItem
{
protected: //чтоб нельзя было вызвать на прямую
	CHudItemObject() = default;
	virtual ~CHudItemObject() override = default;

public:
	virtual DLL_Pure	*_construct			() override;

public:
	virtual CInventoryItem* cast_inventory_item	() override { return this; }
	virtual CCustomDetector* cast_custom_detector() override { return nullptr; }
	virtual CCustomDevice* cast_custom_device() override { return nullptr; }
	virtual CWeaponBinoculars* cast_weapon_binoculars() override { return nullptr; }
	virtual CWeaponKnife* cast_weapon_knife() override { return nullptr; }
	virtual CWeaponMagazined* cast_weapon_magazined() override { return nullptr; }
	virtual CWeaponMagazinedWGrenade* cast_weapon_magazined_w_grenade() override { return nullptr; }
	virtual CWeaponBM16* cast_weapon_bm16() override { return nullptr; }
	virtual CWeapon* cast_weapon() override {return nullptr;}
	virtual CWeaponRPG7* cast_weapon_rpg7() override { return nullptr; }
	virtual CWeaponRG6* cast_weapon_rg6() override { return nullptr; }
	virtual CHudItem* cast_hud_item() override { return this; }
	virtual CGrenade* cast_grenade() override { return nullptr; }
	virtual CMissile* cast_missile() override { return nullptr; }
	virtual CBolt* cast_bolt() override { return nullptr; }
	virtual CPhysicsShellHolder* cast_physics_shell_holder() override { return this; }
	virtual CPhysicItem* cast_physics_item() override { return this; }
	virtual CWeaponShotgun* cast_weapon_shotgun() override { return nullptr; }

public:
	virtual void		Load				(const char* section) override;
	virtual bool		Action				(u16 cmd, u32 flags) override;
	virtual void		SwitchState			(u8 S) override;
	virtual void		OnStateSwitch		(u8 S) override;
	virtual void		OnEvent				(NET_Packet& P, u16 type) override;
	virtual void		OnH_A_Chield		() override;
	virtual void		OnH_B_Chield		() override;
	virtual void		OnH_B_Independent	(bool just_before_destroy) override;
	virtual void		OnH_A_Independent	() override;
	virtual bool		net_Spawn			(CSE_Abstract* DC) override;
	virtual void		net_Destroy			() override;
	virtual bool		ActivateItem		() override;
	virtual void		DeactivateItem	() override;
	virtual void		UpdateCL			() override;
	virtual void		renderable_Render	() override;
	virtual void		on_renderable_Render() override;
	virtual void		OnMoveToRuck		(const SInvItemPlace& prev) override;

	virtual void Serialize(ISaveObject& Object) override { CInventoryItemObject::Serialize(Object); }

	virtual bool			use_parent_ai_locations	() const override
	{
		return				CInventoryItemObject::use_parent_ai_locations	() && (Device.dwFrame != dwXF_Frame);
	}
};
