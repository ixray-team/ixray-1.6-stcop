#pragma once
#include "../state.h"


class CStateMonsterDangerMoveToHomePoint : public CState {
protected:
	typedef CState		inherited;
	typedef CState*	state_ptr;

	u32					m_target_node;
	bool				m_skip_camp;
	Fvector				m_danger_pos;

public:
						CStateMonsterDangerMoveToHomePoint(CBaseMonster *obj);
	virtual	void		initialize				();
	virtual void 		finalize				();
	virtual void 		critical_finalize		();

	virtual bool		check_start_conditions	();
	virtual bool		check_completion		();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}

	virtual	void		reselect_state			();
	virtual	void		setup_substates			();
private:
			Fvector		&get_most_danger_pos	();
};
