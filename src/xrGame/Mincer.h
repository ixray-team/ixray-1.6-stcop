/////////////////////////////////////////////////////
// Аномальная зона: "мясорубка"
// При попадании живого объекта в зону происходит 
// электрический разряд
// Зона восстанавливает заряд через определенное время
// (через m_dwPeriod заряжается с 0 до m_fMaxPower)
//
/////////////////////////////////////////////////////
#pragma once

#include "GraviZone.h"
#include "TeleWhirlwind.h"
#include "PhysicsShellHolder.h"
#include "../xrScripts/script_export_space.h"
#include "PHDestroyable.h"

class CMincer final : public CBaseGraviZone, public CPHDestroyableNotificator
{
	typedef CBaseGraviZone inherited;
	shared_str m_torn_particles;
	ref_sound m_tearing_sound;
	float m_fActorBlowoutRadiusPercent;

public:
	CMincer();
	virtual ~CMincer() override;

	//	virtual void SwitchZoneState(EZoneState new_state);
	virtual void OnStateSwitch(EZoneState new_state) override;
	virtual bool feel_touch_contact(CObject* O) override;
	virtual void feel_touch_new(CObject* O) override;
	virtual void Load(const char* section) override;
	virtual bool BlowoutState() override;

	virtual void AffectPullDead(CPhysicsShellHolder* GO, const Fvector& throw_in_dir, float dist) override
	{
	}

	virtual void AffectPullAlife(CEntityAlive* EA, const Fvector& throw_in_dir, float dist) override;
	virtual void AffectThrow(SZoneObjectInfo* O, CPhysicsShellHolder* GO, const Fvector& throw_in_dir, float dist) override;
	virtual void ThrowInCenter(Fvector& C) override;
	virtual bool net_Spawn(CSE_Abstract* DC) override;
	virtual void net_Destroy() override;
	virtual void Center(Fvector& C) const override;
	virtual void NotificateDestroy(CPHDestroyableNotificate* dn) override;
	virtual float BlowoutRadiusPercent(CPhysicsShellHolder* GO) override;
	virtual CMincer* cast_mincer_zone() override { return this; }

	virtual CTeleWhirlwind* telekinesis() override { return this; }
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
