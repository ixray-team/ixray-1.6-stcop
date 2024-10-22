#pragma once
#include "../../../pp_effector_custom.h"

class CPPEffectorPsyDogAura : public CPPEffectorCustom 
{
protected:
	using inherited = CPPEffectorCustom;
	
	enum {eStateFadeIn, eStateFadeOut, eStatePermanent} m_effector_state;
	
	u32				m_time_state_started;
	u32				m_time_to_fade;

public:
					CPPEffectorPsyDogAura	(const SPPInfo &ppi, u32 time_to_fade);
					~CPPEffectorPsyDogAura();

	virtual BOOL	update					();
			void	switch_off				();
};


