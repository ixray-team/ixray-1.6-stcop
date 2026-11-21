#pragma once
#include "ArmorBase.h"

struct SBoneProtections;
class CTacticalFlashLaser;

XREFLECT(LuaExport, Bases="CGameObject", Constructible)
class CHelmet :
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

	virtual void		Load					(const char* section) override;

	virtual void		OnMoveToSlot			(const SInvItemPlace& previous_place) override;
	virtual void		OnMoveToRuck			(const SInvItemPlace& previous_place) override;

	virtual bool can_be_attached() const override;
	virtual void OnAttachmentInc(PIItem item) override;
	virtual void OnAttachmentDec(PIItem item) override;

	virtual CHelmet*	cast_helmet				() override { return this; }

	float m_fShowNearestEnemiesDistance = 0.0f;
	xr_vector<CTacticalFlashLaser*> m_lights;

protected:
	virtual bool install_upgrade_impl			(const char* section, bool test) override;
};
