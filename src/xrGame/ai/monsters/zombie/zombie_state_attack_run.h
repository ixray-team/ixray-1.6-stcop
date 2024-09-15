#pragma once

#include "../state.h"


class CStateZombieAttackRun : public CState {
	typedef CState inherited;

	TTime				m_time_action_change;
	EAction				action;

public:
						CStateZombieAttackRun	(CBaseMonster *obj);
	virtual				~CStateZombieAttackRun	();

	virtual void		initialize				();
	virtual	void		execute					();

	virtual bool 		check_completion		();
	virtual bool 		check_start_conditions	();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}

private:
			void		choose_action			();

};

#include "zombie_state_attack_run_inline.h"
