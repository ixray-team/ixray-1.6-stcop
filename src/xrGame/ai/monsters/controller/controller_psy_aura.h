#pragma once
#include "../../../pp_effector_custom.h"

class CControllerBase;

struct SAuraSound {
	ref_sound	left;
	ref_sound	right;
};

class CControllerAura : public CPPEffectorCustomController<CPPEffectorControllerAura>{
protected:
	using inherited = CPPEffectorCustomController<CPPEffectorControllerAura>;

	CControllerBase			*m_object;
	u32					m_time_last_update;

	SAuraSound			aura_sound;
	float				aura_radius;
	
	u32					m_time_fake_aura;

	u32					m_time_fake_aura_duration;
	u32					m_time_fake_aura_delay;
	float				m_fake_max_add_dist;
	float				m_fake_min_add_dist;

	u32					m_time_started;

public:
	CControllerAura(CControllerBase* object);
	~CControllerAura() = default;

	virtual void	load					(LPCSTR section) override;

			void	on_death				();
			void	update_schedule			();
			void	update_frame			();
};


