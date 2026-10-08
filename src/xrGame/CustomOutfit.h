#pragma once

#include "ArmorBase.h"
#include "Descs/OutfitDesc.h"
#include "../xrScripts/script_export_space.h"

struct SBoneProtections;

class CCustomOutfit :
	public CArmorBase
{
	using inherited = CArmorBase;
public:

	virtual void Load(const char* section) override;

	//коэффициент на который домножается потеря силы
	//если на персонаже надет костюм
	float			GetPowerLoss				();

	virtual void	OnMoveToSlot				(const SInvItemPlace& prev) override final;
	virtual void	OnMoveToRuck				(const SInvItemPlace& previous_place) override final;

	virtual CCustomOutfit* cast_outfit			() override final { return this; }

	virtual u32	ef_equipment_type				() const override final;
	virtual	bool BonePassBullet					(u16 boneID) override final;
	const shared_str& GetFullIconName			() const { return OutfitDesc().m_full_icon_name; }
	u32	get_artefact_count						() const { return m_artefact_count; }
	void ApplySkinModel							(CActor* pActor, bool bDress, bool bHUDOnly);

	shared_str GetPortrait						() const { return OutfitDesc().m_character_portrait; }

	const SCustomOutfitDesc& OutfitDesc			() const { VERIFY(CurrentOutfitDesc); return *CurrentOutfitDesc; }
	bool IsHelmetAvailable						() const { return OutfitDesc().bIsHelmetAvaliable; }

protected:
	const SCustomOutfitDesc* CurrentOutfitDesc = nullptr;
	u32	m_artefact_count = 0;

public:
	float m_additional_weight = 0.0f;
	float m_additional_weight2 = 0.0f;

protected:
	virtual bool install_upgrade_impl(const char* section, bool test) override final;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
