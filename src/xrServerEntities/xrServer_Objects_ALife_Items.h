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

#include "InfoPortionDefs.h"

#pragma warning(push)
#pragma warning(disable:4005)

class CSE_ALifeItemAmmo;

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeInventoryItem, IPureStateUpdateObject)
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
									CSE_ALifeInventoryItem	(const char* caSection);
virtual ~CSE_ALifeInventoryItem	() override;
	// we need this to prevent virtual inheritance :-(
	virtual CSE_Abstract			*base					() { return nullptr; }
	virtual const CSE_Abstract		*base					() const { return nullptr; }
	virtual CSE_Abstract			*init					();
	virtual CSE_Abstract			*cast_abstract			() {return 0;};
	virtual CSE_ALifeInventoryItem	*cast_inventory_item	() {return this;};
	virtual	u32						update_rate				() const;
	virtual bool					Net_Relevant			();

			bool					has_upgrade				(const shared_str& upgrade_id);
			void					add_upgrade				(const shared_str& upgrade_id);
			void					SetCondition(float cond) {
				m_fCondition = cond;
			}
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
		return						(base()->ID_Parent < ALife::INVALID_OBJECT_ID);
	}
	virtual bool					bfUseful();

	/////////// network ///////////////
	u8								m_u8NumItems;
	SPHNetState						State;
	///////////////////////////////////
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN2(CSE_ALifeItem,CSE_ALifeDynamicObjectVisual,CSE_ALifeInventoryItem)
	bool							m_physics_disabled;

									CSE_ALifeItem	(const char* caSection);
virtual ~CSE_ALifeItem	() override;
virtual CSE_Abstract			*base			() override;
virtual const CSE_Abstract		*base			() const override;
virtual CSE_Abstract			*init					() override;
virtual CSE_Abstract			*cast_abstract			() override {return this;};
virtual CSE_ALifeInventoryItem	*cast_inventory_item	() override {return this;};
virtual bool					Net_Relevant			() override;
virtual void					OnEvent					(NET_Packet &tNetPacket, u16 type, u32 time, ClientID sender ) override;
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemTorch,CSE_ALifeItem)
//флаги
	enum EStats{
		eTorchActive				= (1<<0),
		eNightVisionActive			= (1<<1),
		eAttached					= (1<<2)
	};
	bool							m_active;
	bool							m_nightvision_active;
	bool							m_attached;
									CSE_ALifeItemTorch	(const char* caSection);
virtual ~CSE_ALifeItemTorch	() override;
virtual bool					Net_Relevant			() override;

SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemAmmo,CSE_ALifeItem)
	u16								a_elapsed;
	u16								m_boxSize;

									CSE_ALifeItemAmmo	(const char* caSection);
virtual ~CSE_ALifeItemAmmo	() override;
virtual CSE_ALifeItemAmmo		*cast_item_ammo		() override {return this;};
virtual bool					can_switch_online	() const override;
virtual bool					can_switch_offline	() const override;
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemFuel, CSE_ALifeItem)
	CSE_ALifeItemFuel(LPCSTR caSection);
virtual ~CSE_ALifeItemFuel() override;
virtual CSE_ALifeItemFuel* cast_item_fuel() override { return this; }
virtual bool					can_switch_online() const override;
virtual bool					can_switch_offline() const override;
SERVER_ENTITY_DECLARE_END


SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemWeapon,CSE_ALifeItem)

	typedef	ALife::EWeaponAddonStatus	EWeaponAddonStatus;
	
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
	u8								wpn_flags = 0;
	u8								wpn_state = 0;
	u8								chamber_ammo_type = 0;
	u16								a_current = 0;
	u16								a_chamber_elapsed = 0;
	u8								misfire = 0;
	float							rt_zoom_factor = 0.0f;
	u8								cur_scope = 0;

	struct ammo_elapsed_t
	{
		union
		{
			u16 data = 0;

			struct
			{
				u16 MagazineElapsed : 8;
				u16 GrenadesElapsed : 8;
			};
		};
	} a_elapsed;

	struct ammo_type_t
	{
		union
		{
			u8 data = 0;

			struct
			{
				u8 MagazinedType : 4;
				u8 GrenadeType : 4;
			};
		};
	} a_ammo_type;

	float							m_fHitPower;
	ALife::EHitType					m_tHitType;
	const char*							m_caAmmoSections;
	u32								m_dwAmmoAvailable;
	Flags8							m_addon_flags;
	u8								m_bZoom = false;
	u32								m_ef_main_weapon_type;
	u32								m_ef_weapon_type;

									CSE_ALifeItemWeapon	(const char* caSection);
virtual ~CSE_ALifeItemWeapon() override;
virtual void					OnEvent				(NET_Packet& P, u16 type, u32 time, ClientID sender ) override;
virtual u32						ef_main_weapon_type	() const override;
virtual u32						ef_weapon_type		() const override;
	u8								get_slot			();
	u16								get_ammo_limit		();
	u16								get_ammo_total		();
	u16								get_ammo_elapsed	();
	u16								get_ammo_magsize	();
	void							clone_addons		(CSE_ALifeItemWeapon* parent);
	void							clone_upgrades		(CSE_ALifeItemWeapon* parent);

virtual bool					Net_Relevant		() override;

virtual CSE_ALifeItemWeapon		*cast_item_weapon	() override {return this;}

#ifndef XRGAME_EXPORTS
	void OnEditorMagazineTypeChanged(PropValue*);
	void OnEditorMagazineElapsedChanged(PropValue*);
#endif // #ifndef XRGAME_EXPORTS
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemFlamethrower, CSE_ALifeItemWeapon)
	CSE_ALifeItemFlamethrower(LPCSTR caSection);
virtual ~CSE_ALifeItemFlamethrower() override;

virtual CSE_ALifeItemFlamethrower* cast_item_flamethrower() override { return this; }
SERVER_ENTITY_DECLARE_END


SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemWeaponMagazined,CSE_ALifeItemWeapon)
u8			m_u8CurFireMode;
CSE_ALifeItemWeaponMagazined(const char* caSection);
virtual ~CSE_ALifeItemWeaponMagazined() override;

virtual CSE_ALifeItemWeapon		*cast_item_weapon	() override {return this;}
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemWeaponMagazinedWGL, CSE_ALifeItemWeaponMagazined)
bool			m_bGrenadeMode = false;
CSE_ALifeItemWeaponMagazinedWGL(const char* caSection);
virtual ~CSE_ALifeItemWeaponMagazinedWGL() override;

virtual CSE_ALifeItemWeapon		*cast_item_weapon	() override {return this;}
virtual CSE_ALifeItemWeaponMagazinedWGL	*cast_item_weapon_magazined_wgl	() override {return this;}
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemWeaponShotGun,CSE_ALifeItemWeaponMagazined)
	xr_vector<u8>				m_AmmoIDs;
								CSE_ALifeItemWeaponShotGun(const char* caSection);
virtual ~CSE_ALifeItemWeaponShotGun() override;

virtual CSE_ALifeItemWeapon		*cast_item_weapon	() override {return this;}
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemWeaponAutoShotGun,CSE_ALifeItemWeaponShotGun)
								CSE_ALifeItemWeaponAutoShotGun(const char* caSection);
virtual ~CSE_ALifeItemWeaponAutoShotGun() override;

virtual CSE_ALifeItemWeapon		*cast_item_weapon	() override {return this;}
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemDetector,CSE_ALifeItem)
	u32								m_ef_detector_type;
									CSE_ALifeItemDetector(const char* caSection);
virtual ~CSE_ALifeItemDetector() override;
virtual u32						ef_detector_type() const override;
virtual CSE_ALifeItemDetector	*cast_item_detector		() override {return this;}
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemArtefact,CSE_ALifeItem)
	float							m_fAnomalyValue;
									CSE_ALifeItemArtefact	(const char* caSection);
virtual ~CSE_ALifeItemArtefact	() override;
virtual bool					Net_Relevant			() override;
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemPDA,CSE_ALifeItem)
	ALife::_OBJECT_ID				m_original_owner;
	shared_str						m_specific_character;
	shared_str						m_info_portion;

									CSE_ALifeItemPDA(const char* caSection);
virtual ~CSE_ALifeItemPDA() override;
virtual CSE_ALifeItemPDA		*cast_item_pda				() override {return this;};
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemDocument,CSE_ALifeItem)
	shared_str							m_wDoc;
									CSE_ALifeItemDocument(const char* caSection);
virtual ~CSE_ALifeItemDocument() override;
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemGrenade,CSE_ALifeItem)
	u32								m_ef_weapon_type;
									CSE_ALifeItemGrenade	(const char* caSection);
virtual ~CSE_ALifeItemGrenade	() override;
virtual u32						ef_weapon_type			() const override;
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemExplosive,CSE_ALifeItem)
									CSE_ALifeItemExplosive(const char* caSection);
virtual ~CSE_ALifeItemExplosive() override;
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemBolt,CSE_ALifeItem)
	u32								m_ef_weapon_type;
									CSE_ALifeItemBolt	(const char* caSection);
virtual ~CSE_ALifeItemBolt	() override;
virtual bool					can_save			() const override;
virtual bool					used_ai_locations	() const override;
virtual u32						ef_weapon_type		() const override;
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemsNotSave, CSE_ALifeItem)
	u32								m_ef_weapon_type;
									CSE_ALifeItemsNotSave(const char* caSection);
virtual ~CSE_ALifeItemsNotSave() override;
virtual bool					can_save() const override;
virtual bool					used_ai_locations() const override;
virtual u32						ef_weapon_type() const override;
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemCustomOutfit,CSE_ALifeItem)
	u32								m_ef_equipment_type;
									CSE_ALifeItemCustomOutfit	(const char* caSection);
virtual ~CSE_ALifeItemCustomOutfit	() override;
virtual u32						ef_equipment_type			() const override;
virtual bool					Net_Relevant				() override;
SERVER_ENTITY_DECLARE_END

SERVER_ENTITY_DECLARE_BEGIN(CSE_ALifeItemHelmet,CSE_ALifeItem)
									CSE_ALifeItemHelmet	(const char* caSection);
virtual ~CSE_ALifeItemHelmet	() override;
virtual bool					Net_Relevant			() override;
SERVER_ENTITY_DECLARE_END

#pragma warning(pop)

#endif