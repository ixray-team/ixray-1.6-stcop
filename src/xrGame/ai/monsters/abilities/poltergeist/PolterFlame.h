#pragma once

#include "../xrEngine/CameraManager.h"
#include "PolterAbility.h"

class CPolterFlame final : public CPolterSpecialAbility {

	typedef CPolterSpecialAbility inherited;

	ref_sound				m_sound;
	LPCSTR					m_particles_prepare;
	LPCSTR					m_particles_fire;
	LPCSTR					m_particles_stop;
	u32						m_time_fire_delay;
	u32						m_time_fire_play;

	float					m_length;
	float					m_hit_value;
	u32						m_hit_delay;

	u32						m_count;
	u32						m_delay;	// between 2 flames

	u32						m_time_flame_started;

	float					m_min_flame_dist;
	float					m_max_flame_dist;
	float					m_min_flame_height;
	float					m_max_flame_height;

	float					m_pmt_aura_radius;


	// Scanner
	float					m_scan_radius;
	u32						m_scan_delay_min;
	u32						m_scan_delay_max;

	SPPInfo					m_scan_effector_info;
	float					m_scan_effector_time;
	float					m_scan_effector_time_attack;
	float					m_scan_effector_time_release;
	ref_sound				m_scan_sound;

	bool					m_state_scanning;
	u32						m_scan_next_time;


	enum EFlameState {
		ePrepare,
		eFire,
		eStop
	};


public:
	struct SFlameElement {
		const CObject* target_object;
		Fvector				position;
		Fvector				target_dir;
		u32					time_started;
		ref_sound			sound;
		CParticlesObject* particles_object;
		EFlameState			state;
		u32					time_last_hit;
	};


private:

	xr_vector<SFlameElement*>			m_flames;

public:
	CPolterFlame(IPolterInterface* polter);
	~CPolterFlame() override;

	void	load(LPCSTR section) override;
	void	update_schedule() override;
	void	on_destroy() override;
	void	on_die() override;

private:
	void	select_state(SFlameElement* elem, EFlameState state);
	bool	get_valid_flame_position(const CObject* target_object, Fvector& res_pos);
	void	create_flame(const CObject* target_object);
};
