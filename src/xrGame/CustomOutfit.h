#pragma once

#include "ArmorBase.h"
#include "../xrScripts/script_export_space.h"

struct SBoneProtections;

class CCustomOutfit :
	public CArmorBase
{
	using inherited = CArmorBase;
public:

	virtual void Load(LPCSTR section) override;

	//коэффициент на который домножается потеря силы
	//если на персонаже надет костюм
	float			GetPowerLoss				();

	virtual void	OnMoveToSlot				(const SInvItemPlace& prev) override final;
	virtual void	OnMoveToRuck				(const SInvItemPlace& previous_place) override final;

	virtual CCustomOutfit* cast_outfit			() override final { return this; }

	virtual u32	ef_equipment_type				() const override final;
	virtual	BOOL BonePassBullet					(int boneID) override final;
	const shared_str& GetFullIconName			() const { return m_full_icon_name; }
	u32	get_artefact_count						() const { return m_artefact_count; }
	void ApplySkinModel							(CActor* pActor, bool bDress, bool bHUDOnly);

	shared_str GetPortrait						() const { return m_character_portrait; }

protected:
	shared_str m_full_icon_name;
	shared_str m_character_portrait;
	u32	m_ef_equipment_type = 0;
	u32	m_artefact_count = 0;

public:
	shared_str m_ActorVisual;
	float m_additional_weight = 0.0f;
	float m_additional_weight2 = 0.0f;

	bool bIsHelmetAvaliable = true;
	bool IsExo = false;
	bool IsExoProto = false;
	bool isDisableChangeSkin = true;

protected:
	virtual bool install_upgrade_impl(LPCSTR section, bool test) override final;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
