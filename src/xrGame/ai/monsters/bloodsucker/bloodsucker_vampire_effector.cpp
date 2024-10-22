#include "stdafx.h"
#include "bloodsucker_vampire_effector.h"

CustomBloodsuckerVampirePPEffector::CustomBloodsuckerVampirePPEffector(const SPPInfo &ppi, float life_time) :
	inherited(EEffectorPPType(eCEHit), life_time)
{
	state		= ppi;
	m_total		= life_time;
}

CustomBloodsuckerVampirePPEffector::~CustomBloodsuckerVampirePPEffector()
{

}

BOOL CustomBloodsuckerVampirePPEffector::Process(SPPInfo& pp)
{
    inherited::Process(pp);

    float time_past_perc = (m_total - fLifeTime) / m_total;

    float factor = 0.f;

    if (time_past_perc < EntityDefinitions::CBloodsuckerBase::TimeAttack)
    {
        factor = 0.75f * time_past_perc / EntityDefinitions::CBloodsuckerBase::TimeAttack;
    }
    else if (time_past_perc > (1 - EntityDefinitions::CBloodsuckerBase::TimeAttack))
    {
        factor = 0.75f * (1 - time_past_perc) / EntityDefinitions::CBloodsuckerBase::TimeAttack;
    }
    else
    {
        float time_past_sine_perc = 
            (time_past_perc - EntityDefinitions::CBloodsuckerBase::TimeAttack) * (1 / (1 - EntityDefinitions::CBloodsuckerBase::TimeAttack +
                EntityDefinitions::CBloodsuckerBase::TimeAttack));

        factor = 0.5f + 0.25f * std::sin(EntityDefinitions::CBloodsuckerBase::PercToRad(time_past_sine_perc));
    }

    factor = std::clamp(factor, 0.01f, 1.0f);
    pp.lerp(pp_identity, state, factor);

    return TRUE;
}


