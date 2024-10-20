#pragma once
#include "../../../pp_effector_custom.h"

class CActor;
class CPseudoPsyDogBase;

class CPsyDogAura : public CPPEffectorCustomController<CPPEffectorPsyDogAura>
{
	
	CPseudoPsyDogBase					*m_object;
	CActor					*m_actor;

	u32						m_time_actor_saw_phantom;
	u32						m_time_phantom_saw_actor;

public:
					CPsyDogAura						(CPseudoPsyDogBase *dog) : m_object(dog){}
			void	reinit							();
			void	on_death						();
			void	update_schedule					();
};