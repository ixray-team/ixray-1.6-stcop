#pragma once
#include "IViewport.h"

class CViewportParticle :
    public IViewport
{
public:
    CViewportParticle();
    virtual ~CViewportParticle();

    virtual void Draw() override;
    virtual void Render() override;
    void OpenModel(PS::CPGDef* Part);
    void OpenModel(PS::CPEDef* Part);

private:
    UIRenderForm View;

    PS::CParticleGroup* ParticleGroupView = nullptr;
    PS::CParticleEffect* ParticleEffectView = nullptr;
};