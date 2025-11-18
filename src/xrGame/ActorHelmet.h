#pragma once

#include "inventory_item_object.h"

struct SBoneProtections;

class CHelmet final : public CInventoryItemObject
{
private:
	using inherited = CInventoryItemObject;
public:
	CHelmet();
	virtual	~CHelmet();

	virtual void Load(LPCSTR section) override;

	void Hit(float P, ALife::EHitType hit_type);

	shared_str m_BonesProtectionSect;
	shared_str m_NightVisionSect;

	virtual void OnMoveToSlot(const SInvItemPlace& previous_place) override;
	virtual void OnMoveToRuck(const SInvItemPlace& previous_place) override;
	virtual BOOL net_Spawn(CSE_Abstract* DC) override;
	virtual void net_Export(NET_Packet& P) override;
	virtual void net_Import(NET_Packet& P) override;
	virtual void OnH_A_Chield() override;

	float GetDefHitTypeProtection(ALife::EHitType hit_type);
	float GetHitTypeProtection(ALife::EHitType hit_type, s16 element);
	float GetBoneArmor(s16 element);

	float HitThroughArmor(float hit_power, s16 element, float ap, bool& add_wound, ALife::EHitType hit_type);

	bool GlassPresent = false;

	float m_fPowerLoss = 0.0f;
	float m_fHealthRestoreSpeed = 0.0f;
	float m_fRadiationRestoreSpeed = 0.0f;
	float m_fSatietyRestoreSpeed = 0.0f;
	float m_fThirstRestoreSpeed = 0.0f;
	float m_fPowerRestoreSpeed = 0.0f;
	float m_fBleedingRestoreSpeed = 0.0f;

	bool bIsHudGasMaskAvialable = false;		// FFx0001 ++
	bool bIsHudRainDropsAvialable = false;	// FFx0001 ++

	float m_fShowNearestEnemiesDistance = 0.0f;

	void ReloadBonesProtection();
	void AddBonesProtection(LPCSTR bones_section);

	virtual CHelmet* cast_helmet() override { return this; }

protected:
	HitImmunity::HitTypeSVec m_HitTypeProtection = {};
	SBoneProtections* m_boneProtection = nullptr;

protected:
	virtual bool install_upgrade_impl(LPCSTR section, bool test) override;
};
