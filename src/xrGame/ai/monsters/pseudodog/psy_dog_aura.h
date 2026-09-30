#pragma once
////////////////////////////////////////////////////////////////////////
// Effector controlling class
////////////////////////////////////////////////////////////////////////
#include "../../../pp_effector_custom.h"
#include "../../../CActorAuraPostEffectsBalancer.h"

class CPPEffectorPsyDogAura : public CPPEffectorCustom {
	typedef CPPEffectorCustom inherited;
	
	enum {eStateFadeIn, eStateFadeOut, eStatePermanent} m_effector_state;
	
	u32				m_time_state_started;
	u32				m_time_to_fade;

public:
					CPPEffectorPsyDogAura	(const SPPInfo &ppi, u32 time_to_fade);
	virtual bool	update					();
			void	switch_off				();
};

class CPsyDog;
class CActor;

class CPsyDogAura : public CPPEffectorCustomController<CPPEffectorPsyDogAura>{
	
	CPsyDog					*m_object;
	CActor					*m_actor;

	u32						m_time_actor_saw_phantom;
	u32						m_time_phantom_saw_actor;

	shared_str				m_section;

public:
					CPsyDogAura						(CPsyDog *dog) : m_object(dog){}
			void	reinit							();
			void	on_death						();
			void	update_schedule					();
	virtual	void	load							(const char* section);
};

