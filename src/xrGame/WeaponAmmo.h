#pragma once
#include "inventory_item_object.h"
#include "anticheat_dumpable_object.h"
#include "../xrScripts/script_export_space.h"
#include "CartrigeParam.h"
#include "RepackerInterface.h"

class CCartridge final : 
	public IAnticheatDumpable
{
public:
	CCartridge();
	void Load(const char* section, u8 LocalAmmoType);
	float Weight() const;

	shared_str	m_ammoSect;
	enum{
		cfTracer				= (1<<0),
		cfRicochet				= (1<<1),
		cfCanBeUnlimited		= (1<<2),
		cfExplosive				= (1<<3),
		cfMagneticBeam			= (1<<4),
	};
	SCartridgeParam param_s;

	u8		m_LocalAmmoType;
	bool 	m_4to1_tracer;

	u16		bullet_material_idx;
	Flags8	m_flags;

	shared_str	m_InvShortName;

	const char* GetInventoryName() { return m_InvShortName.c_str(); };
	virtual void				DumpActiveParams		(shared_str const & section_name, CInifile & dst_ini) const;
	virtual shared_str const 	GetAnticheatSectionName	() const { return m_ammoSect; };
};

class CWeaponAmmo final :
	public CInventoryItemObject,
	public IRepackerInterface
{
	using inherited = CInventoryItemObject;
public:
	CWeaponAmmo() = default;
	virtual ~CWeaponAmmo() = default;

	virtual CWeaponAmmo* cast_weapon_ammo() {return this;}
	virtual IRepackerInterface* cast_repacker_interface() override {return this;}
	virtual void					Load				(const char* section);
	virtual bool					net_Spawn			(CSE_Abstract* DC);
	virtual void					net_Destroy			();
	virtual void					net_Export			(NET_Packet& P);
	virtual void					net_Import			(NET_Packet& P);
	virtual void					OnH_B_Chield		();
	virtual void					OnH_B_Independent	(bool just_before_destroy);
	virtual void					UpdateCL			();
	virtual void					renderable_Render	();

	virtual bool					Useful				() const;
	virtual float					Weight				() const;
	virtual	u32						Cost				() const;

	bool							Get					(CCartridge &cartridge);
	
	virtual bool Repack(PIItem Other) override;
	virtual bool IsValid() const override;

	SCartridgeParam cartridge_param;

	u16			m_boxSize;
	u16			m_boxCurr;
	bool		m_tracer;
	bool 		m_4to1_tracer;

public:
	virtual CInventoryItem *can_make_killing	(const CInventory *inventory) const;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
