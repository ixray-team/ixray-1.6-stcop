//////////////////////////////////////////////////////////////////////
// ExplosiveItem.cpp:	класс для вещи которая взрывается под 
//						действием различных хитов (канистры,
//						балоны с газом и т.д.)
//////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "ExplosiveItem.h"

void CExplosiveItem::Load(const char* section)
{
	inherited::Load(section);
	CExplosive::Load(section);

	m_flags.set(FUsingCondition, true);
	CDelayedActionFuse::Initialize(pSettings->r_float(section, "time_to_explode"), pSettings->r_float(section, "condition_to_explode"));
	VERIFY(pSettings->line_exist(section, "set_timer_particles"));

	// Added by Axel, to enable optional condition use on any item
	m_flags.set(FUsingCondition, READ_IF_EXISTS(pSettings, r_bool, section, "use_condition", true));
}

void CExplosiveItem::net_Destroy()
{
	inherited::net_Destroy();
	CExplosive::net_Destroy();
}

void CExplosiveItem::Hit(SHit* pHDS)
{
	if (CDelayedActionFuse::isActive())
	{
		pHDS->power = 0.0f;
	}

	inherited::Hit(pHDS);

	VERIFY(pHDS->who);

	if (!CDelayedActionFuse::isActive() && pHDS->who != nullptr && CDelayedActionFuse::CheckCondition(GetCondition()))
	{
		//запомнить того, кто взорвал вещь
		SetInitiator(pHDS->who->ID());
	}
}

void CExplosiveItem::StartTimerEffects()
{
	TParticlesPlayer* PPlayer = GetOrCreateComponent<TParticlesPlayer>();
	PPlayer->StartParticles(pSettings->r_string(*cNameSect(), "set_timer_particles"), Fvector().set(0, 1, 0), ID());
}

void CExplosiveItem::OnEvent(NET_Packet& P, u16 type)
{
	CExplosive::OnEvent(P, type);
	inherited::OnEvent(P, type);
}

void CExplosiveItem::UpdateCL()
{
	CExplosive::UpdateCL();
	inherited::UpdateCL();
}

void CExplosiveItem::shedule_Update(u32 dt)
{
	inherited::shedule_Update(dt);
	if (CDelayedActionFuse::isActive() && CDelayedActionFuse::Update(GetCondition()))
	{
		Fvector normal;
		FindNormal(normal);
		CExplosive::GenExplodeEvent(Position(), normal);

		TParticlesPlayer* PPlayer = GetOrCreateComponent<TParticlesPlayer>();
		PPlayer->StopParticles(ID(), BI_NONE, true);
	}
}

bool CExplosiveItem::shedule_Needed()
{
	return (inherited::shedule_Needed() || CDelayedActionFuse::isActive());
}

void CExplosiveItem::renderable_Render()
{
	inherited::renderable_Render();
}

void CExplosiveItem::net_Relcase(CObject* O)
{
	CExplosive::net_Relcase(O);
	inherited::net_Relcase(O);
}

void CExplosiveItem::ActivateExplosionBox(const Fvector& size, Fvector& in_out_pos)
{}

void CExplosiveItem::GetRayExplosionSourcePos(Fvector& pos)
{
	random_point_in_object_box(pos, this);
}