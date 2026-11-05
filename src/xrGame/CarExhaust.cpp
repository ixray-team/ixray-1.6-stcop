#include "stdafx.h"
#ifdef DEBUG
#include "ode_include.h"
#include "../xrEngine/StatGraph.h"
#include "PHDebug.h"
#endif
#include "alife_space.h"
#include "hit.h"
#include "PHDestroyable.h"
#include "car.h"
#include "../Include/xrRender/Kinematics.h"
#include "PHWorld.h"


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
CCar::SExhaust::~SExhaust()
{
	Particles::Details::Destroy(p_pgobject);
	Particles::Details::Destroy(p_pgobject_nitro);
}

void CCar::SExhaust::Init()
{
	VERIFY(!physics_world()->Processing());

	const char* ExhaustParticles = pcar->m_exhaust_particles.c_str();
	if (ExhaustParticles != nullptr && ExhaustParticles[0] != '\0')
	{
		p_pgobject = Particles::Details::Create(ExhaustParticles, false);
		if (p_pgobject)
		{
			p_pgobject->UpdateParent(pcar->XFORM(), zero_vel);
		}
	}

	const char* NitroParticles = pcar->m_nitro_particles.c_str();
	if (pcar->m_nitro && pcar->b_nitro_particles_enabled && NitroParticles != nullptr && NitroParticles[0] != '\0')
	{
		p_pgobject_nitro = Particles::Details::Create(NitroParticles, false);
		if (p_pgobject_nitro)
		{
			p_pgobject_nitro->UpdateParent(pcar->XFORM(), zero_vel);
		}
	}
}

void CCar::SExhaust::Update()
{
	if (!p_pgobject) return;
	Fvector velocity = zero_vel;
	pcar->m_pPhysicsShell->get_LinearVel(velocity);

	CBoneInstance& BI = PKinematics(pcar->Visual())->LL_GetBoneInstance(bone_id);
	Fmatrix BI_M;
	BI_M = BI.mTransform;
    BI_M.mulA_43(pcar->XFORM());
	Fmatrix BI_N = BI_M;

	float exhausts_scale = (0.5f + pcar->m_engine_rpm_scale * 0.5f) * (1.f + pcar->m_engine_drive);

	Fvector particle_c = BI_N.c;
	BI_M.k.mul(exhausts_scale);
	BI_M.j.mul(exhausts_scale);
	BI_M.i.mul(exhausts_scale);
	BI_M.c = particle_c;

	p_pgobject->UpdateParent(BI_M, velocity);

	if (pcar->m_nitro && p_pgobject_nitro)
	{
		if (pcar->m_nitro_particle_scale != 1.f)
		{
			Fvector nitro_c = BI_N.c;
			BI_N.k.mul(pcar->m_nitro_particle_scale);
			BI_N.j.mul(pcar->m_nitro_particle_scale);
			BI_N.i.mul(pcar->m_nitro_particle_scale);
			BI_N.c = nitro_c;
		}
		p_pgobject_nitro->UpdateParent(BI_N, velocity);
	}
}

void CCar::SExhaust::Clear()
{
	Particles::Details::Destroy(p_pgobject);
	Particles::Details::Destroy(p_pgobject_nitro);
}

void CCar::SExhaust::Play()
{
	VERIFY(!physics_world()->Processing());

	if(!p_pgobject) return;
	if(p_pgobject->IsPlaying())
		p_pgobject->Stop();

	Update();
	p_pgobject->Play(false);
}

void CCar::SExhaust::Stop()
{
	VERIFY(!physics_world()->Processing());

	if(!p_pgobject) return;
	if(!p_pgobject->IsPlaying())
		return;

	p_pgobject->Stop();
}

void CCar::SExhaust::PlayNitro()
{
	VERIFY(!physics_world()->Processing());

	if(!p_pgobject_nitro) return;
	if(p_pgobject_nitro->IsPlaying())
		p_pgobject_nitro->Stop();

	Update();
	p_pgobject_nitro->Play(false);
}

void CCar::SExhaust::StopNitro()
{
	VERIFY(!physics_world()->Processing());

	if(!p_pgobject_nitro) return;
	if(!p_pgobject_nitro->IsPlaying())
		return;

	p_pgobject_nitro->Stop();
}

void CCar::SExhaust::PlayPop()
{
	VERIFY(!physics_world()->Processing());

	Fvector velocity = zero_vel;
	pcar->m_pPhysicsShell->get_LinearVel(velocity);

	CBoneInstance& BI = PKinematics(pcar->Visual())->LL_GetBoneInstance(bone_id);
	Fmatrix BI_M;
	BI_M = BI.mTransform;
	BI_M.mulA_43(pcar->XFORM());

	const char* PopParticles = pcar->m_pop_particles.c_str();
	if (PopParticles == nullptr || PopParticles[0] == '\0')
	{
		return;
	}

	Level().ph_commander().add_call(new CPHOnesCondition(), new CCarParticlesPlayCall(BI_M, velocity, PopParticles));
}
