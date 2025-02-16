#include "stdafx.h"
#include "zombie_choke_effector.h"

//////////////////////////////////////////////////////////////////////////
// Choke PPE Effector by SkyLoader (based on vampire effector)
//////////////////////////////////////////////////////////////////////////

CChokePPEffector::CChokePPEffector(const SPPInfo &ppi, float life_time) :
	inherited(EEffectorPPType(eCEHit), life_time)
{
	state		= ppi;
	m_total		= life_time;
}

#define TIME_ATTACK		0.2f
#define PERIODS			2			
#define RAD_TO_PERC(rad)	((rad - PI_DIV_2) / (PERIODS * PI_MUL_2))
#define PERC_TO_RAD(perc)	(perc * (PERIODS * PI_MUL_2) + PI_DIV_2)

BOOL CChokePPEffector::Process(SPPInfo& pp)
{
	inherited::Process(pp);

	// amount of time passed in percents
	float time_past_perc = (m_total - fLifeTime) / m_total;
	
	float factor;
	if (time_past_perc < TIME_ATTACK) {
		factor = 0.75f * time_past_perc / TIME_ATTACK;
	} else if (time_past_perc > (1 - TIME_ATTACK)) {
		factor = 0.75f * (1-time_past_perc) / TIME_ATTACK;
	} else {	
		float time_past_sine_perc = (time_past_perc - TIME_ATTACK) * (1 / ( 1 - TIME_ATTACK + TIME_ATTACK));
		factor = 0.5f + 0.25f * _sin(PERC_TO_RAD(time_past_sine_perc));
	}
	
	clamp(factor,0.01f,1.0f);
	pp.lerp				(pp_identity, state, factor);

	return TRUE;
}