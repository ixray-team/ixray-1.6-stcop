///////////////////////////////////////////////////////////////////////////
//  Базовый класс: Кровосос ТЧ
//	Мутант: Кровосос (Матёрый)
//  Заметка: Использует логику всех трех классов: ЗП/ТЧ/ЧН
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../../../../../xrEngine/effectorPP.h"
#include "../../../../CameraEffector.h"
#include "../../../../../xrEngine/cameramanager.h"

class CVampirePPEffectsocor : 
    public CEffectorPP
{
    using inherited = CEffectorPP;

    SPPInfo state; // current state
    float m_total; // total PP time

public:
    CVampirePPEffectsocor(const SPPInfo& ppi, float life_time);
    virtual BOOL Process(SPPInfo& pp);
};

//////////////////////////////////////////////////////////////////////////
// Vampire Camera Effector
//////////////////////////////////////////////////////////////////////////
class CVampireCameraEffectorsoc : 
    public CEffectorCam
{
    using inherited = CEffectorCam;

    float m_time_total;
    Fvector dangle_target;
    Fvector dangle_current;

    float m_dist;
    Fvector m_direction;

public:
    CVampireCameraEffectorsoc(float time, const Fvector& src, const Fvector& tgt);
    virtual BOOL ProcessCam(SCamEffectorInfo& info);
};
