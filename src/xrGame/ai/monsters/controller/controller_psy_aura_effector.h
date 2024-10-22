#pragma once
#include "../../../pp_effector_custom.h"

class CPPEffectorControllerAura : public CPPEffectorCustom
{
protected:
	using inherited = CPPEffectorCustom;

	enum { eStateFadeIn, eStateFadeOut, eStatePermanent } m_effector_state;

	u32				m_time_state_started;
	u32				m_time_to_fade;

	ref_sound		m_snd_left;
	ref_sound		m_snd_right;

public:
	CPPEffectorControllerAura(const SPPInfo& ppi, u32 time_to_fade, const ref_sound& snd_left, const ref_sound& snd_right);
	virtual ~CPPEffectorControllerAura() override;

	virtual BOOL	update() override;
	void			switch_off();
};