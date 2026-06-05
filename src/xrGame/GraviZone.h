//////////////////////////////////////////////////////////////////////////
// GraviZone.h:		гравитационна€ аномали€
//////////////////////////////////////////////////////////////////////////
//					состоит как бы из 2х зон
//					одна зат€гивает объект, друга€ взрывает и 
//					все неживые объекты (предметы и трупы)
//					поднимает в воздух и качает там какое-то
//					врем€
//////////////////////////////////////////////////////////////////////////

#pragma once
#include "AnomalyZone.h"
#include "ai/monsters/telekinesis.h"


class CBaseGraviZone : public CAnomalyZone
{
	using inherited = CAnomalyZone;

public:
	CBaseGraviZone();
	virtual ~CBaseGraviZone();

	virtual void Load(const char* section);

	virtual bool net_Spawn(CSE_Abstract* DC);
	virtual void net_Destroy();
	virtual void net_Relcase(CObject* O);

	//воздействие зоной на объект
	virtual void Affect(SZoneObjectInfo* O);
	virtual void AffectPull(CPhysicsShellHolder* GO, const Fvector& throw_in_dir, float dist);
	virtual void AffectPullAlife(CEntityAlive* EA, const Fvector& throw_in_dir, float dist);
	virtual void AffectPullDead(CPhysicsShellHolder* GO, const Fvector& throw_in_dir, float dist);
	virtual void AffectThrow(SZoneObjectInfo* O, CPhysicsShellHolder* GO, const Fvector& throw_in_dir, float dist);
	virtual void ThrowInCenter(Fvector& C);
	virtual bool CheckAffectField(CPhysicsShellHolder* GO, float dist_to_radius);
	virtual void shedule_Update(u32 dt);
	virtual bool BlowoutState();
	virtual bool IdleState();

	virtual float BlowoutRadiusPercent(CPhysicsShellHolder* /*GO*/) { return m_fBlowoutRadiusPercent; }

	CBaseGraviZone* cast_base_gravi_zone() override { return this; }

protected:
	virtual CTelekinesis& Telekinesis() =0;

	//сила импульса вт€гивани€ в зону (дл€ веса 100 кг)
	float m_fThrowInImpulse;
	//сила импульса вт€гивани€ в зону дл€ живых существ
	float m_fThrowInImpulseAlive;
	//коэфф. зат€гивани€ (чем меньше, тем плавнее зат€гивает)
	float m_fThrowInAtten;
	//радиус действи€ выброса (в процентах от всего)
	float m_fBlowoutRadiusPercent;

	//параметры телекинеза	
	float m_fTeleHeight;
	u32 m_dwTimeToTele;
	u32 m_dwTelePause;
	u32 m_dwTeleTime;

	//им€ партиклов телекинеза
	void PlayTeleParticles(CGameObject* pObject);
	void StopTeleParticles(CGameObject* pObject);

	shared_str m_sTeleParticlesBig;
	shared_str m_sTeleParticlesSmall;
};

class CGraviZone final : public CBaseGraviZone
{
	typedef CBaseGraviZone inherited;
	CTelekinesis m_telekinesis;

protected:
	virtual CTelekinesis& Telekinesis() { return m_telekinesis; }

public:
	CGraviZone()
	{
	}

	virtual ~CGraviZone()
	{
	}

	CGraviZone* cast_gravi_zone() override { return this; }
};
