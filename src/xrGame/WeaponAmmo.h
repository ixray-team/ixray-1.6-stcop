#pragma once
#include "inventory_item_object.h"
#include "anticheat_dumpable_object.h"
#include "../xrScripts/script_export_space.h"
#include "RepackerInterface.h"

XREFLECT(LuaExport, Constructible)
struct SCartridgeParam
{
	XPROPERTY(LuaReadWrite, Name="kDist ")
	float	kDist;
	XPROPERTY(LuaReadWrite, Name="kDisp ")
	float	kDisp;
	XPROPERTY(LuaReadWrite, Name="kHit ")
	float	kHit;
	XPROPERTY(LuaReadWrite, Name="kImpulse ")
	float	kImpulse;
	XPROPERTY(LuaReadWrite, Name="kAP ")
	float	kAP;
	XPROPERTY(LuaReadWrite, Name="kAirRes ")
	float	kAirRes;
	XPROPERTY(LuaReadWrite)
	int		buckShot;
	XPROPERTY(LuaReadWrite)
	float	impair;
	float	fWallmarkSize;
	XPROPERTY(LuaReadWrite)
	u8		u8ColorID;

	XPROPERTY(LuaCallable)
	IC void Init()
	{
		kDist = kDisp = kHit = kImpulse = 1.0f;
		kAP       = 0.0f;
		kAirRes   = 0.0f;
		buckShot  = 1;
		impair    = 1.0f;
		fWallmarkSize = 0.0f;
		u8ColorID     = 0;
	}
};

XREFLECT(LuaExport, Constructible)
class CCartridge final : 
	public IAnticheatDumpable
{
public:
	CCartridge();
	void Load(const char* section, u8 LocalAmmoType);
	XPROPERTY(LuaCallable)
	float Weight() const;

	shared_str	m_ammoSect;
	XPROPERTY(LuaEnum, Name="cartridge_flags")
	enum ECartridgeFlags{
		cfTracer				= (1<<0),
		cfRicochet				= (1<<1),
		cfCanBeUnlimited		= (1<<2),
		cfExplosive				= (1<<3),
		cfMagneticBeam			= (1<<4),
	};
	XPROPERTY(LuaReadWrite)
	SCartridgeParam param_s;

	XPROPERTY(LuaReadWrite)
	u8		m_LocalAmmoType;
	XPROPERTY(LuaReadWrite)
	bool 	m_4to1_tracer;

	XPROPERTY(LuaReadWrite)
	u16		bullet_material_idx;
	XPROPERTY(LuaReadWrite)
	Flags8	m_flags;

	shared_str	m_InvShortName;

	XPROPERTY(LuaCallable)
	const char* GetInventoryName() { return m_InvShortName.c_str(); };
	virtual void				DumpActiveParams		(shared_str const & section_name, CInifile & dst_ini) const override;
	virtual shared_str const 	GetAnticheatSectionName	() const override { return m_ammoSect; };
};

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CWeaponAmmo final : 
	public CInventoryItemObject,
	public IRepackerInterface
{
	using inherited = CInventoryItemObject;
public:
	CWeaponAmmo() = default;
	virtual ~CWeaponAmmo() override = default;

	virtual CWeaponAmmo				*cast_weapon_ammo	() override {return this;}
	virtual IRepackerInterface* cast_repacker_interface() override {return this;}
	virtual void					Load				(const char* section) override;
	virtual bool					net_Spawn			(CSE_Abstract* DC) override;
	virtual void					net_Destroy			() override;
	virtual void					net_Export			(NET_Packet& P) override;
	virtual void					net_Import			(NET_Packet& P) override;
	virtual void					OnH_B_Chield		() override;
	virtual void					OnH_B_Independent	(bool just_before_destroy) override;
	virtual void					UpdateCL			() override;
	virtual void					renderable_Render	() override;

	virtual bool					Useful				() const override;
	XPROPERTY(LuaCallable)
	virtual float					Weight				() const override;
	XPROPERTY(LuaCallable)
	virtual u32						Cost				() const override;

	XPROPERTY(LuaCallable)
	bool							Get					(CCartridge &cartridge);

	void SetBoxCurr(u16 Count);

	virtual bool Repack(PIItem Other) override;
	virtual bool IsValid() const override;
	
	SCartridgeParam cartridge_param;

	XPROPERTY(LuaReadWrite)
	u16			m_boxSize;
	XPROPERTY(LuaReadWrite)
	u16			m_boxCurr;
	XPROPERTY(LuaReadWrite)
	bool		m_tracer;
	XPROPERTY(LuaReadWrite)
	bool 		m_4to1_tracer;

public:
	virtual CInventoryItem *can_make_killing	(const CInventory *inventory) const override;
};
