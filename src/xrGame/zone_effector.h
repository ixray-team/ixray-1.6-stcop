#pragma once

#include "../xrEngine/AI/alife_space.h"
#include "CActorAuraPostEffectsBalancer.h"

class CActor;

class CZoneEffector {
	float						r_min_perc;
	float						r_max_perc;
	float						m_factor;
	shared_str					m_pp_section;
	u32							m_object_id;
	EAuraPostEffectType			m_type;

public:
	CActor*						m_pActor;

			CZoneEffector		();
			~CZoneEffector		();

	void	Load				(const char* section);
	void	Update				(u32 object_id, float dist, float radius, ALife::EHitType hit_type, EAuraPostEffectType aura_type);
	void	Stop				();

private:
	void	Activate			();
};
