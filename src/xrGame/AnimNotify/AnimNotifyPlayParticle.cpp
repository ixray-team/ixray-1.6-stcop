#include "stdafx.h"
#include "AnimNotifyPlayParticle.h"

#include "../Include/xrRender/Kinematics.h"
#include "../Include/xrRender/RenderVisual.h"

#include "../xrParticles/stdafx.h"
#include "../xrParticles/ParticlesObject.h"

constexpr u32 m_particles_num = 20;

void CAnimNotifyPlayParticle::Construct(const CInifile& ini, LPCSTR sect)
{
    m_particle_to_play = ini.r_string(sect, "particle");
}

void CAnimNotifyPlayParticle::Execute(IRenderVisual* visual, u16 bone_id)
{
    auto Kinematics = visual->dcast_PKinematics();
    Fmatrix pos;
    pos.set(Kinematics->LL_GetBoneInstance(bone_id).mTransform);

    auto NewParticles = Particles::Details::Create(m_particle_to_play.c_str());
    NewParticles->SetXFORM			(pos);
    NewParticles->Play(false);
}
