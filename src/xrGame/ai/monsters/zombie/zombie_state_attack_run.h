#pragma once

#include "../state.h"

class CStateZombieAttackRun : public CState 
{
protected:
	using inherited = CState;

	TTime				m_time_action_change;
	EAction				action;

public:
						CStateZombieAttackRun	(CBaseMonster* object);
	virtual				~CStateZombieAttackRun	() override;

	virtual void		initialize				() override;
	virtual	void		execute					() override;

	virtual bool 		check_completion		() override;
	virtual bool 		check_start_conditions	() override;
	virtual void		remove_links			(CObject* object) override { inherited::remove_links(object);}

private:
			void		choose_action			();
};
