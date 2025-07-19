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

class CCustomDetector;
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

class CInventoryItemObject : 
			public CInventoryItem, 
			public CPhysicItem
{
public:
							CInventoryItemObject	();
	virtual					~CInventoryItemObject	();
	virtual DLL_Pure		*_construct				();

public:
	virtual CPhysicsShellHolder*cast_physics_shell_holder	()	{return this;}
	virtual CInventoryItem	*cast_inventory_item			()	{return this;}
	virtual CAttachableItem	*cast_attachable_item			()	{return this;}
	virtual CGameObject		*cast_game_object				()  {return this;}
	virtual CHudItem		*cast_hud_item					()	{return nullptr;}
	virtual CCustomDetector	*cast_custom_detector			()	{return nullptr;}
	virtual CWeaponBinoculars*cast_weapon_binoculars		()  {return nullptr;}
	virtual CWeaponKnife	*cast_weapon_knife				()  {return nullptr;}
	virtual CWeaponMagazined*cast_weapon_magazined			()  {return nullptr;}
	virtual CWeaponMagazinedWGrenade* cast_weapon_magazined_w_grenade() {return nullptr;}
	virtual CWeaponBM16		*cast_weapon_bm16				()  {return nullptr;}
	virtual CWeapon			*cast_weapon					()  {return nullptr;}
	virtual CTorch			*cast_torch						()  {return nullptr;}
	virtual CWeaponRPG7* cast_weapon_rpg7() { return nullptr; }
	virtual CWeaponRG6* cast_weapon_rg6() { return nullptr; }
	virtual CBolt* cast_bolt() { return nullptr; }
	virtual CPda* cast_pda() { return nullptr; }
	virtual CGrenade* cast_grenade() { return nullptr; }
	virtual CMissile* cast_missile() { return nullptr; }
	virtual CSilencer* cast_addon_silencer() {return nullptr;}
	virtual CScope* cast_addon_scope() {return nullptr;}
	virtual CGrenadeLauncher* cast_addon_grenade_launcher() {return nullptr;}
	virtual CPhysicItem* cast_physics_item() { return this; }

public:
	virtual void	Load					(LPCSTR section);
//*	virtual LPCSTR	Name					();
//*	virtual LPCSTR	NameShort				();
	virtual	void	Hit						(SHit* pHDS);

	virtual void	OnH_B_Independent		(bool just_before_destroy);
	virtual void	OnH_A_Independent		();
	virtual void	OnH_B_Chield			();
	virtual void	OnH_A_Chield			();
	virtual void	UpdateCL				();
	virtual void	OnEvent					(NET_Packet& P, u16 type);
	virtual BOOL	net_Spawn				(CSE_Abstract* DC);
	virtual void	net_Destroy				();
	virtual void	net_Import				(NET_Packet& P);					// import from server
	virtual void	net_Export				(NET_Packet& P);					// export to server
	virtual void	save					(NET_Packet &output_packet);
	virtual void	load					(IReader &input_packet);
	virtual BOOL	net_SaveRelevant		()								{return TRUE;}
	virtual void	renderable_Render		(IDSGraphManager* DM = nullptr);
	virtual void	reload					(LPCSTR section);
	virtual void	reinit					();
	virtual void	activate_physic_shell	();
	virtual void	on_activate_physic_shell();
	virtual	void	modify_holder_params			(float &range, float &fov) const;
public:
	////////// network //////////////////////////////////////////////////
	virtual void	make_Interpolation		();
	virtual void	PH_B_CrPr				(); // actions & operations before physic correction-prediction steps
	virtual void	PH_I_CrPr				(); // actions & operations after correction before prediction steps
#ifdef DEBUG
	virtual void	PH_Ch_CrPr				(); // 
#endif
	virtual void	PH_A_CrPr				(); // actions & operations after phisic correction-prediction steps
	virtual bool	NeedToDestroyObject		() const;

protected:
#ifdef DEBUG_DRAW
	virtual void	OnRender				();
#endif

public:
	virtual bool	Useful					() const;

public:
	virtual u32		ef_weapon_type			() const;
protected:
	virtual bool	use_parent_ai_locations	() const
	{
		return CAttachableItem::use_parent_ai_locations();
	}

};

#include "inventory_item_object_inline.h"
