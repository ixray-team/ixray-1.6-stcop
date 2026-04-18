#pragma once

#include "inventory_item_object.h"
#include "antigas.h"

struct SBoneProtections;

class CArmorBase : public CInventoryItemObject, public IAntigas
{
private:
	typedef	CInventoryItemObject inherited;

public:
							CArmorBase				();
	virtual					~CArmorBase				();

	virtual CArmorBase* cast_armorbase() override final { return this; }

	virtual void UpdateCL() override;

	virtual void			Hit						(float P, ALife::EHitType hit_type);
	virtual void			Load					(const char* section);

	virtual void			ReloadBonesProtection	();
	virtual bool			install_upgrade_impl	(const char* section, bool test);
	virtual void			save(NET_Packet& packet) override;
	virtual void			load(IReader& packet) override;
	virtual float			HitThroughArmor			(float hit_power, s16 element, float ap, bool& add_wound, ALife::EHitType hit_type);
	virtual float			GetDefHitTypeProtection	(ALife::EHitType hit_type);
	virtual float			GetHitTypeProtection	(ALife::EHitType hit_type, s16 element);
	virtual float			GetBoneArmor			(s16 element);
	virtual void			AddBonesProtection		(const char* bones_section);
	virtual bool			net_Spawn				(CSE_Abstract* DC);
	virtual void			net_Export				(NET_Packet& P);
	virtual void			net_Import				(NET_Packet& P);
	virtual void			OnH_A_Chield			();
	virtual shared_str		GetNV_Sect				() const { return m_NightVisionSect; }
	virtual bool			IsHudGasMaskAvailable	() { return bIsHudGasMaskAvailable; }
	virtual bool			IsHudRainDropsAvailable	() { return bIsHudRainDropsAvailable; }
	void OverrideHitTypeProtection(ALife::EHitType hit_type, float value);
	bool InstallAntigasFilter(CInventoryItem* inventory_item);
	bool UnInstallAntigasFilter();

	bool IsTorchAvailable() const { return m_bTorchAvailable; }

	bool					GlassPresent = false;
	float					m_fPowerLoss = 0.0f;
	float					m_fHealthRestoreSpeed = 0.0f;
	float					m_fRadiationRestoreSpeed = 0.0f;
	float					m_fSatietyRestoreSpeed = 0.0f;
	float					m_fThirstRestoreSpeed = 0.0f;
	float					m_fPowerRestoreSpeed = 0.0f;
	float					m_fBleedingRestoreSpeed = 0.0f;

protected:
	HitImmunity::HitTypeSVec m_HitTypeProtection;
	SBoneProtections*		m_boneProtection;

	bool					bIsHudGasMaskAvailable = false;		// FFx0001 ++
	bool					bIsHudRainDropsAvailable = false;	// FFx0001 ++

	shared_str				m_BonesProtectionSect;
	shared_str				m_NightVisionSect;

	bool					m_bTorchAvailable = false;
};