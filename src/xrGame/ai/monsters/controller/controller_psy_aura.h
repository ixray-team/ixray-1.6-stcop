#pragma once
#include "../../../pp_effector_custom.h"

class CController;

struct SAuraSound {
	ref_sound	left;
	ref_sound	right;
};


class CPPEffectorControllerAura : public CPPEffectorCustom {
	typedef CPPEffectorCustom inherited;

	enum { eStateFadeIn, eStateFadeOut, eStatePermanent } m_effector_state;

	u32				m_time_state_started;
	u32				m_time_to_fade;

	ref_sound		m_snd_left;
	ref_sound		m_snd_right;

public:
	CPPEffectorControllerAura(const SPPInfo& ppi, u32 time_to_fade, const ref_sound& snd_left, const ref_sound& snd_right);
	~CPPEffectorControllerAura();
	virtual BOOL	update();
	void			switch_off();
	void			stop_snds();
};

class CControllerAura : public CPPEffectorCustomController<CPPEffectorControllerAura> {
	typedef CPPEffectorCustomController<CPPEffectorControllerAura> inherited;

	CController* m_object;
	u32					m_time_last_update;
	//tatarinrafa: Lets use a bug with r2_aa on dx10 or 11 as a cool effect which happens when mblur is off
	//bool				hack_mblur_controll;

	SAuraSound			aura_sound;

	float				aura_radius_min;
	float				aura_radius_max;
	float				aura_radius_max_y;
	float				aura_damage;
	float				aura_effector_max_factor;

	float				current_effector_strength;

	LPCSTR				aura_regular_effector_sect;
	LPCSTR				aura_hit_effector_sect;

	u32					m_time_fake_aura;

	// min/max fake aura duration
	Ivector2			m_time_fake_aura_duration;
	// min/max fake aura delay
	Ivector2			m_time_fake_aura_delay;

	//bool				b_do_double_vision_effect;

	// hits
	enum {
		eNone,
		eEffectoring,
		eHit
	} m_hit_state;

	u32					m_time_started;
	u32					m_pmt_hit_delay;
	u32					m_pmt_pp_hit_delay;


public:
	CControllerAura(CController* monster) : m_object(monster) {}
	virtual ~CControllerAura();

	virtual void	load(LPCSTR section);

	void	on_destroy();
	void	on_death();
	void	update_schedule();
	void	update_frame();
	float   xr_stdcall get_effector_strength();
};