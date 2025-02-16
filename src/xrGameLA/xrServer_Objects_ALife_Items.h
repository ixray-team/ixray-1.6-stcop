////////////////////////////////////////////////////////////////////////////
//	Module 		: xrServer_Objects_ALife.h
//	Created 	: 19.09.2002
//  Modified 	: 04.06.2003
//	Author		: Oles Shyshkovtsov, Alexander Maksimchuk, Victor Reutskiy and Dmitriy Iassenev
//	Description : Server objects items for ALife simulator
////////////////////////////////////////////////////////////////////////////

#ifndef xrServer_Objects_ALife_ItemsH
#define xrServer_Objects_ALife_ItemsH

#include "xrServer_Objects_ALife.h"
#include "PHSynchronize.h"
#include "inventory_space.h"

#include "character_info_defs.h"
#include "infoportiondefs.h"

#pragma warning(push)
#pragma warning(disable:4005)

class CSE_ALifeItemAmmo;

SERVER_ENTITY_DECLARE_BEGIN0(CSE_ALifeInventoryItem)
public:
	enum {
		inventory_item_state_enabled	= u8(1) << 0,
		inventory_item_angular_null		= u8(1) << 1,
		inventory_item_linear_null		= u8(1) << 2,
	};

	union mask_num_items {
		struct {
			u8	num_items : 5;
			u8	mask      : 3;
		};
		u8		common;
	};

public:
	float							m_fCondition;
	float							m_fMass;
	u32								m_dwCost;
	s32								m_iHealthValue;
	s32								m_iFoodValue;
	float							m_fDeteriorationValue;
	CSE_ALifeObject					*m_self;
	u32								m_last_update_time;
	xr_vector<shared_str>			m_upgrades;

public:
									CSE_ALifeInventoryItem	(LPCSTR caSection);
	virtual							~CSE_ALifeInventoryItem	();
	// we need this to prevent virtual inheritance :-(
	virtual CSE_Abstract			*base					() = 0;
	virtual const CSE_Abstract		*base					() const = 0;
	virtual CSE_Abstract			*init					();
	virtual CSE_Abstract			*cast_abstract			() {return 0;};
	virtual CSE_ALifeInventoryItem	*cast_inventory_item	() {return this;};
	virtual	u32						update_rate				() const;
	virtual BOOL					Net_Relevant			();

			bool					has_upgrade				(const shared_str& upgrade_id);
			void					add_upgrade				(const shared_str& upgrade_id);

private:
	bool							prev_freezed;
	bool							freezed;
					u32				m_freeze_time;
	static const	u32				m_freeze_delta_time;
	static const	u32				random_limit;
					CRandom			m_relevent_random;

public:
	// end of the virtual inheritance dependant code

	IC		bool					attached	() const
	{
		return						(base()->ID_Parent < 0xffff);
	}
	virtual bool					bfUseful();

	/////////// network ///////////////
	u8								m_u8NumItems;
	SPHNetState						State;
	///////////////////////////////////
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeInventoryItem)
#define script_type_list save_type_list(CSE_ALifeInventoryItem)

SERVER_ENTITY_DECLARE_BEGIN2(CSE_ALifeItem,CSE_ALifeDynamicObjectVisual,CSE_ALifeInventoryItem)
	bool							m_physics_disabled;

									CSE_ALifeItem	(LPCSTR caSection);
	virtual							~CSE_ALifeItem	();
	virtual CSE_Abstract			*base			();
	virtual const CSE_Abstract		*base			() const;
	virtual CSE_Abstract			*init					();
	virtual CSE_Abstract			*cast_abstract			() {return this;};
	virtual CSE_ALifeInventoryItem	*cast_inventory_item	() {return this;};
	virtual BOOL					Net_Relevant			();
	virtual void					OnEvent					(NET_Packet &tNetPacket, u16 type, u32 time, ClientID sender );
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItem)
#define script_type_list save_type_list(CSE_ALifeItem)

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemTorch,CSE_ALifeItem)
//флаги
	enum EStats{
		eTorchActive = (1 << 0),
		eNightVisionActive = (1 << 1),
		eAttached = (1 << 2)
	};
	bool							m_active;
	bool							m_attached;
	u16								m_battery_state;
	bool							m_nightvision_active;
									CSE_ALifeItemTorch	(LPCSTR caSection);
    virtual							~CSE_ALifeItemTorch	();
	virtual BOOL					Net_Relevant			();

SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemTorch)
#define script_type_list save_type_list(CSE_ALifeItemTorch)

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemAmmo,CSE_ALifeItem)
	u16								a_elapsed;
	u16								m_boxSize;

									CSE_ALifeItemAmmo	(LPCSTR caSection);
	virtual							~CSE_ALifeItemAmmo	();
	virtual CSE_ALifeItemAmmo		*cast_item_ammo		()  {return this;};
	virtual bool					can_switch_online	() const;
	virtual bool					can_switch_offline	() const;
			u16						get_ammo_left		() const;
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemAmmo)
#define script_type_list save_type_list(CSE_ALifeItemAmmo)

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemWeapon,CSE_ALifeItem)

	//возможность подключения аддонов
	enum EWeaponAddonStatus{
		eAddonDisabled				= 0,	//нельзя присоеденить
		eAddonPermanent				= 1,	//постоянно подключено по умолчанию
		eAddonAttachable			= 2		//можно присоединять
	};

	//текущее состояние аддонов
	enum EWeaponAddonState 
	{
		eWeaponAddonScope = 0x01,
		eWeaponAddonGrenadeLauncher = 0x02,
		eWeaponAddonSilencer = 0x04
	};

	EWeaponAddonStatus				m_scope_status;
	EWeaponAddonStatus				m_silencer_status;				
	EWeaponAddonStatus				m_grenade_launcher_status;

	u32								timestamp;
	u8								wpn_flags;
	u8								wpn_state;
	u8								ammo_type;
	u16								a_current;
	u16								a_elapsed;

	float							m_fHitPower;
	ALife::EHitType					m_tHitType;
	LPCSTR							m_caAmmoSections;
	u32								m_dwAmmoAvailable;
	Flags8							m_addon_flags;
	u8								m_cur_scope;
	u8								m_bZoom;
	u32								m_ef_main_weapon_type;
	u32								m_ef_weapon_type;

									CSE_ALifeItemWeapon	(LPCSTR caSection);
	virtual							~CSE_ALifeItemWeapon();
	virtual void					OnEvent				(NET_Packet& P, u16 type, u32 time, ClientID sender );
	virtual u32						ef_main_weapon_type	() const;
	virtual u32						ef_weapon_type		() const;
	u8								get_slot			() const;
	u16								get_ammo_limit		() const;
	u16								get_ammo_total		() const;
	u16								get_ammo_elapsed	() const;
	void							set_ammo_elapsed	(u16);
	u16								get_ammo_magsize	() const;
	u8								get_addon_flags		() const;
	void							set_addon_flags		(u8 flags)		{ m_addon_flags.assign(flags); }
	void							set_ammo_type		(u8 type)		{ ammo_type = type; }
	u8								get_ammo_type		() const        { return ammo_type; }

	virtual BOOL					Net_Relevant		();

	virtual CSE_ALifeItemWeapon		*cast_item_weapon	() {return this;}
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemWeapon)
#define script_type_list save_type_list(CSE_ALifeItemWeapon)

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemWeaponMagazined,CSE_ALifeItemWeapon)
u8			m_u8CurFireMode;
CSE_ALifeItemWeaponMagazined(LPCSTR caSection);
virtual							~CSE_ALifeItemWeaponMagazined();

virtual CSE_ALifeItemWeapon		*cast_item_weapon	() {return this;}
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemWeaponMagazined)
#define script_type_list save_type_list(CSE_ALifeItemWeaponMagazined)


SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemWeaponMagazinedWGL, CSE_ALifeItemWeaponMagazined)

							CSE_ALifeItemWeaponMagazinedWGL(LPCSTR caSection);
	virtual						~CSE_ALifeItemWeaponMagazinedWGL();

	u8								grndIsLoaded_;
	u8								grndID_;
	bool							grenadeMode_server;

	IC bool							is_grenade_mode		() const	{ return grenadeMode_server; }
	IC void							set_grenades_mode	(bool mode)	{ grenadeMode_server = mode; }
	IC u8							get_grenades_type	() const	{ return grndID_; }
	IC void							set_grenades_type	(u8 type)	{ grndID_ = type; }
	IC u8							get_grenades_count	() const	{ return grndIsLoaded_; }
	IC void							set_grenades_count	(u8 count)	{ grndIsLoaded_ = count; R_ASSERT2(grndIsLoaded_ <= 1, "Implement grenade launcher mag size to work properly with size > 1"); }

	virtual CSE_ALifeItemWeapon		*cast_item_weapon	() {return this;}
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemWeaponMagazinedWGL)
#define script_type_list save_type_list(CSE_ALifeItemWeaponMagazinedWGL)


SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemWeaponShotGun,CSE_ALifeItemWeaponMagazined)
	xr_vector<u8>				m_AmmoIDs;
								CSE_ALifeItemWeaponShotGun(LPCSTR caSection);
virtual							~CSE_ALifeItemWeaponShotGun();

virtual CSE_ALifeItemWeapon		*cast_item_weapon	() {return this;}
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemWeaponShotGun)
#define script_type_list save_type_list(CSE_ALifeItemWeaponShotGun)


SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemDetector,CSE_ALifeItem)
	u32								m_ef_detector_type;
									CSE_ALifeItemDetector(LPCSTR caSection);
	virtual							~CSE_ALifeItemDetector();
	virtual u32						ef_detector_type() const;
	virtual CSE_ALifeItemDetector	*cast_item_detector		() {return this;}
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemDetector)
#define script_type_list save_type_list(CSE_ALifeItemDetector)

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemArtefact,CSE_ALifeItem)
	float							m_fAnomalyValue;
									CSE_ALifeItemArtefact	(LPCSTR caSection);
	virtual							~CSE_ALifeItemArtefact	();
	virtual BOOL					Net_Relevant			();
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemArtefact)
#define script_type_list save_type_list(CSE_ALifeItemArtefact)

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemPDA,CSE_ALifeItem)
	u16								m_original_owner;
	shared_str						m_specific_character;
	shared_str						m_info_portion;

									CSE_ALifeItemPDA(LPCSTR caSection);
	virtual							~CSE_ALifeItemPDA();
	virtual CSE_ALifeItemPDA		*cast_item_pda				() {return this;};
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemPDA)
#define script_type_list save_type_list(CSE_ALifeItemPDA)

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemDocument,CSE_ALifeItem)
	shared_str							m_wDoc;
									CSE_ALifeItemDocument(LPCSTR caSection);
	virtual							~CSE_ALifeItemDocument();
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemDocument)
#define script_type_list save_type_list(CSE_ALifeItemDocument)

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemGrenade,CSE_ALifeItem)
	u32								m_ef_weapon_type;
									CSE_ALifeItemGrenade	(LPCSTR caSection);
	virtual							~CSE_ALifeItemGrenade	();
	virtual u32						ef_weapon_type			() const;
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemGrenade)
#define script_type_list save_type_list(CSE_ALifeItemGrenade)

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemExplosive,CSE_ALifeItem)
									CSE_ALifeItemExplosive(LPCSTR caSection);
	virtual							~CSE_ALifeItemExplosive();
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemExplosive)
#define script_type_list save_type_list(CSE_ALifeItemExplosive)

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemBolt,CSE_ALifeItem)
	u32								m_ef_weapon_type;
									CSE_ALifeItemBolt	(LPCSTR caSection);
	virtual							~CSE_ALifeItemBolt	();
	virtual bool					can_save			() const;
	virtual bool					used_ai_locations	() const;
	virtual u32						ef_weapon_type		() const;
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemBolt)
#define script_type_list save_type_list(CSE_ALifeItemBolt)

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemCustomOutfit,CSE_ALifeItem)
	u32							m_ef_equipment_type;
	bool							m_nightvision_active;

									CSE_ALifeItemCustomOutfit	(LPCSTR caSection);
	virtual							~CSE_ALifeItemCustomOutfit	();
	virtual u32						ef_equipment_type			() const;
	virtual BOOL					Net_Relevant				();
SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeItemCustomOutfit)
#define script_type_list save_type_list(CSE_ALifeItemCustomOutfit)

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeEatableItem, CSE_ALifeItem)

									CSE_ALifeEatableItem	(LPCSTR caSection);
    virtual							~CSE_ALifeEatableItem	();

	u16								numOfPortionsServer_;

SERVER_ENTITY_DECLARE_END
add_to_type_list(CSE_ALifeEatableItem)
#define script_type_list save_type_list(CSE_ALifeEatableItem)

#pragma warning(pop)

#endif