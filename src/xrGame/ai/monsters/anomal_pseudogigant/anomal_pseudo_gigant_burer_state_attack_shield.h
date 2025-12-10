#pragma once
#include "../state.h"

template<typename Object>
class CStateAnomalGigBurerShield : public CState<Object>
{
private:
	typedef				CState<Object>	inherited;

public:
						CStateAnomalGigBurerShield		(Object *obj);

	void		initialize				() override;
	void		execute					() override;
	void		finalize				() override;
	void		critical_finalize		() override;
	void		remove_links			(CObject* object) override { inherited::remove_links(object);}

	bool		check_start_conditions	() override;
	bool		check_completion		() override;

private:
	TTime				m_last_shield_started;
	TTime				m_next_particle_allowed;
	float				m_shield_start_anim_length_sec;
	bool				m_started;
};

#include "anomal_pseudo_gigant_burer_state_attack_shield_inline.h"

