#pragma once

#include "ArmorBase.h"
#include "Descs/OutfitDesc.h"
#include "../xrScripts/script_export_space.h"

struct SBoneProtections;

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CCustomOutfit :
	public CArmorBase
{
	using inherited = CArmorBase;
public:
	XPROPERTY(LuaReadWrite, Name="m_fPowerLoss")
	using CArmorBase::m_fPowerLoss;
	XPROPERTY(LuaReadWrite, Name="m_fHealthRestoreSpeed")
	using IRestoresOwner::m_fHealthRestoreSpeed;
	XPROPERTY(LuaReadWrite, Name="m_fRadiationRestoreSpeed")
	using IRestoresOwner::m_fRadiationRestoreSpeed;
	XPROPERTY(LuaReadWrite, Name="m_fSatietyRestoreSpeed")
	using IRestoresOwner::m_fSatietyRestoreSpeed;
	XPROPERTY(LuaReadWrite, Name="m_fThirstRestoreSpeed")
	using IRestoresOwner::m_fThirstRestoreSpeed;
	XPROPERTY(LuaReadWrite, Name="m_fPowerRestoreSpeed")
	using IRestoresOwner::m_fPowerRestoreSpeed;
	XPROPERTY(LuaReadWrite, Name="m_fBleedingRestoreSpeed")
	using IRestoresOwner::m_fBleedingRestoreSpeed;

	XPROPERTY(LuaCallable, Name="GetBoneArmor")
	IC float script_GetBoneArmor(u16 element) { return GetBoneArmor(element); }
	XPROPERTY(LuaCallable, Name="GetDefHitTypeProtection")
	IC float script_GetDefHitTypeProtection(int hit_type) { return GetDefHitTypeProtection(ALife::EHitType(hit_type)); }
	XPROPERTY(LuaCallable, Name="GetHitTypeProtection")
	IC float script_GetHitTypeProtection(int hit_type, const char* element) { return GetHitTypeProtection(ALife::EHitType(hit_type), s16((intptr_t)element)); }

	virtual void Load(const char* section) override;

	//коэффициент на который домножается потеря силы
	//если на персонаже надет костюм
	float			GetPowerLoss				();

	virtual void	OnMoveToSlot				(const SInvItemPlace& prev) final;
	virtual void	OnMoveToRuck				(const SInvItemPlace& previous_place) final;

	virtual CCustomOutfit* cast_outfit			() final { return this; }

	virtual u32	ef_equipment_type				() const final;
	XPROPERTY(LuaCallable)
	virtual bool BonePassBullet					(u16 boneID) final;
	const shared_str& GetFullIconName			() const { return OutfitDesc().m_full_icon_name; }
	XPROPERTY(LuaCallable)
	u32	get_artefact_count						() const { return m_artefact_count; }
	void ApplySkinModel							(CActor* pActor, bool bDress, bool bHUDOnly);

	shared_str GetPortrait						() const { return OutfitDesc().m_character_portrait; }

	const SCustomOutfitDesc& OutfitDesc			() const { VERIFY(CurrentOutfitDesc); return *CurrentOutfitDesc; }
	XPROPERTY(LuaProperty, Name="bIsHelmetAvaliable")
	bool IsHelmetAvailable						() const { return OutfitDesc().bIsHelmetAvaliable; }

protected:
	const SCustomOutfitDesc* CurrentOutfitDesc = nullptr;
	u32	m_artefact_count = 0;

public:
	XPROPERTY(LuaReadWrite)
	float m_additional_weight = 0.0f;
	XPROPERTY(LuaReadWrite)
	float m_additional_weight2 = 0.0f;

protected:
	virtual bool install_upgrade_impl(const char* section, bool test) final;
};
