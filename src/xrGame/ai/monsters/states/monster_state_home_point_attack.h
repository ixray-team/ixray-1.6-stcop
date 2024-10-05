#pragma once
#include "../state.h"

class CStateMonsterAttackMoveToHomePoint : public CState {
protected:
	typedef CState		inherited;
	typedef CState*	state_ptr;

	u32					m_target_node;
	Fvector				m_target_pos;
	bool				m_skip_camp;
	TTime				m_selected_target_time;

public:
						CStateMonsterAttackMoveToHomePoint(CBaseMonster*obj);

	virtual	void		initialize				();
	virtual void 		finalize				();
	virtual void 		critical_finalize		();
	virtual void		execute					();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}

	virtual bool		check_start_conditions	();
	virtual bool		check_completion		();

private:
			void		select_target			();
			void		clean					();
};
