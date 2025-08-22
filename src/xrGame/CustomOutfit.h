#pragma once

#include "inventory_item_object.h"
#include "../xrScripts/script_export_space.h"

struct SBoneProtections;

class CCustomOutfit : public CInventoryItemObject
{
	using inherited = CInventoryItemObject;
public:
	CCustomOutfit();
	virtual	~CCustomOutfit();

	virtual void Load(LPCSTR section) override final;

	//уменьшенная версия хита, для вызова, когда костюм надет на персонажа
	void Hit(float P, ALife::EHitType hit_type);

	//коэффициенты на которые домножается хит
	//при соответствующем типе воздействия
	//если на персонаже надет костюм
	float GetHitTypeProtection(ALife::EHitType hit_type, s16 element);
	float GetDefHitTypeProtection(ALife::EHitType hit_type);
	float GetBoneArmor(s16 element);

	//коэффициент на который домножается потеря силы
    //если на персонаже надет костюм
    float					GetPowerLoss				();

	float HitThroughArmor(float hit_power, s16 element, float ap, bool& add_wound, ALife::EHitType hit_type);

	virtual void OnMoveToSlot(const SInvItemPlace& prev) override final;
	virtual void OnMoveToRuck(const SInvItemPlace& previous_place) override final;
	virtual void OnH_A_Chield() override final;

	virtual CCustomOutfit* cast_outfit() override final { return this; }

protected:
	HitImmunity::HitTypeSVec m_HitTypeProtection = {};

	shared_str m_full_icon_name;
	shared_str m_character_portrait;
	SBoneProtections* m_boneProtection = nullptr;
protected:
	u32	m_ef_equipment_type = 0;
	u32	m_artefact_count = 0;

public:
	shared_str m_ActorVisual;
	bool IsExo = false;
	bool IsExoProto = false;
	bool GlassPresent = false;

	float m_fPowerLoss = 0.0f;
	float m_additional_weight = 0.0f;
	float m_additional_weight2 = 0.0f;

	float m_fHealthRestoreSpeed = 0.0f;
	float m_fRadiationRestoreSpeed = 0.0f;
	float m_fSatietyRestoreSpeed = 0.0f;
	float m_fThirstRestoreSpeed = 0.0f;
	float m_fPowerRestoreSpeed = 0.0f;
	float m_fBleedingRestoreSpeed = 0.0f;

	shared_str m_BonesProtectionSect;
	shared_str m_NightVisionSect;

	bool bIsHelmetAvaliable = true;
	bool bIsHudGasMaskAvialable = false;		// FFx0001 ++
	bool bIsHudRainDropsAvialable = false;	// FFx0001 ++
	bool isDisableChangeSkin = true;

	virtual u32	ef_equipment_type() const override final;
	virtual	BOOL BonePassBullet(int boneID) override final;
	const shared_str& GetFullIconName() const { return m_full_icon_name; }
	u32	get_artefact_count() const { return m_artefact_count; }

	virtual BOOL net_Spawn(CSE_Abstract* DC) override final;
	virtual void net_Export(NET_Packet& P) override final;
	virtual void net_Import(NET_Packet& P) override final;
	void ApplySkinModel(CActor* pActor, bool bDress, bool bHUDOnly);
	void ReloadBonesProtection();
	void AddBonesProtection(LPCSTR bones_section);

	shared_str GetPortrait() const { return m_character_portrait; }

protected:
	virtual bool install_upgrade_impl(LPCSTR section, bool test) override final;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
