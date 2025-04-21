#pragma once
#include "../xrCore/AnimNotify/AnimNotifyTypes.h"

class CParticlesObject;

class CAnimNotifyPlayParticle : public IAnimNotify
{
    shared_str m_particle_to_play;
public:
    void Construct(const CInifile& ini, LPCSTR sect) override;
    void Execute(IRenderVisual* visual, u16 bone_id) override;
    
};
