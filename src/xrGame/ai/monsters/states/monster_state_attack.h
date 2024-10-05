#pragma once

#include "../state.h"
#include "../../../ai_debug.h"

class	CStateMonsterAttack : public CState {
protected:
	typedef CState	inherited;
	typedef CState*	state_ptr;

	u32					m_time_next_run_away;
	u32					m_time_start_check_behinder;
	u32					m_time_start_behinder;

public:
						CStateMonsterAttack		(CBaseMonster *obj);
						CStateMonsterAttack		(CBaseMonster*obj, state_ptr state_move2home);
						CStateMonsterAttack		(CBaseMonster*obj, state_ptr state_run, state_ptr state_melee);
	virtual				~CStateMonsterAttack	();
	
	virtual void		initialize				();
	virtual	void		execute					();
	virtual void		setup_substates			();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}

protected:
			bool		check_steal_state		();
			bool		check_find_enemy_state	();
			bool		check_run_away_state	();
			bool		check_run_attack_state	();
			bool		check_camp_state		();
			bool		check_home_point		();
			bool		check_behinder			();
};
