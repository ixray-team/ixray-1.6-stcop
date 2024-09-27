#pragma once
#include "../state.h"


class CStateGroupAttackMoveToHomePoint : public CState {
protected:
	using inherited = CState	;
	using state_ptr = CState*	;

	u32					m_target_node;
	bool				m_skip_camp;

	TTime				m_first_tick_enemy_inaccessible;
	TTime				m_last_tick_enemy_inaccessible;
	TTime				m_state_started;

public:
						CStateGroupAttackMoveToHomePoint(CBaseMonster * object);
						virtual ~CStateGroupAttackMoveToHomePoint();

	virtual	void		initialize				();
	virtual void 		finalize				();
	virtual void 		critical_finalize		();
	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}

	virtual bool		check_start_conditions	();
	virtual bool		check_completion		();

	virtual	void		reselect_state			();
	virtual	void		setup_substates			();

			bool		enemy_inaccessible		();
};
