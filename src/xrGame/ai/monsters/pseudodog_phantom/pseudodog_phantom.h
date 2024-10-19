#pragma once
#include "../../ai_entity_definitions.h"
#include "../../../../xrScripts/script_export_space.h"

class CPseudoPsyDogBase;
class CPseudoPsyDogPhantomBase;

class CPseudoPsyDogPhantomBase : public CPseudoDogBase 
{
protected:
	using inherited = CPseudoDogBase;
	
	CPseudoPsyDogBase			*m_parent;

	enum 
	{
		eWaitToAppear,
		eAttack
	}m_state;

	SAttackEffector m_appear_effector;

	LPCSTR			m_particles_appear;
	LPCSTR			m_particles_disappear;

	u16				m_parent_id;
		
	u32				m_time_spawned;

public:
	CPseudoPsyDogPhantomBase();
	virtual			~CPseudoPsyDogPhantomBase() override;

	virtual BOOL	net_Spawn			(CSE_Abstract *dc) override;
	virtual void	Think				() override;
	virtual	void	Hit					(SHit* pHDS) override;

	virtual void	net_Destroy			() override;
	virtual void	Die					(CObject* who) override;

			void	destroy_from_parent	();

private:
			void	destroy_me					();
			void	try_to_register_to_parent	();
			bool	is_wait_to_destroy_object	() {return (m_parent_id == 0xffff);}

	DECLARE_SCRIPT_REGISTER_FUNCTION
};