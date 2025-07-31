#pragma once

#include "inventory_item_object.h"
#include "HudItem.h"

class CCustomDetector;
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

class CHudItemObject : 
		public CInventoryItemObject,
		public CHudItem
{
protected: //чтоб нельзя было вызвать на прямую
						CHudItemObject		();
	virtual				~CHudItemObject		();

public:
	virtual	DLL_Pure	*_construct			();

public:
	virtual CInventoryItem* cast_inventory_item	() { return this; }
	virtual CCustomDetector* cast_custom_detector() { return nullptr; }
	virtual CWeaponBinoculars* cast_weapon_binoculars() { return nullptr; }
	virtual CWeaponKnife* cast_weapon_knife() { return nullptr; }
	virtual CWeaponMagazined* cast_weapon_magazined() { return nullptr; }
	virtual CWeaponMagazinedWGrenade* cast_weapon_magazined_w_grenade() { return nullptr; }
	virtual CWeaponBM16* cast_weapon_bm16() { return nullptr; }
	virtual CWeapon* cast_weapon() {return nullptr;}
	virtual CWeaponRPG7* cast_weapon_rpg7() { return nullptr; }
	virtual CWeaponRG6* cast_weapon_rg6() { return nullptr; }
	virtual CHudItem* cast_hud_item() { return this; }
	virtual CGrenade* cast_grenade() { return nullptr; }
	virtual CMissile* cast_missile() { return nullptr; }
	virtual CBolt* cast_bolt() { return nullptr; }
	virtual CPhysicsShellHolder* cast_physics_shell_holder() { return this; }
	virtual CPhysicItem* cast_physics_item() { return this; }

public:
	virtual void		Load				(LPCSTR section);
	virtual bool		Action				(u16 cmd, u32 flags);
	virtual void		SwitchState			(u32 S);
	virtual void		OnStateSwitch		(u32 S);
	virtual void		OnEvent				(NET_Packet& P, u16 type);
	virtual void		OnH_A_Chield		();
	virtual void		OnH_B_Chield		();
	virtual void		OnH_B_Independent	(bool just_before_destroy);
	virtual void		OnH_A_Independent	();
	virtual	BOOL		net_Spawn			(CSE_Abstract* DC);
	virtual void		net_Destroy			();
	virtual bool		ActivateItem		();
	virtual void		DeactivateItem	();
	virtual void		UpdateCL			();
	virtual void		renderable_Render	();
	virtual void		on_renderable_Render();
	virtual void		OnMoveToRuck		(const SInvItemPlace& prev);

	virtual bool			use_parent_ai_locations	() const
	{
		return				CInventoryItemObject::use_parent_ai_locations	() && (Device.dwFrame != dwXF_Frame);
	}
};
