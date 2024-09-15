#pragma once
#include "../state.h"

class CStateBurerAttackGravi : public CState {
	typedef CState inherited;	
public:
							CStateBurerAttackGravi	(CBaseMonster *obj);

		virtual	void		initialize				();
		virtual	void		execute					();
		virtual void		finalize				();
		virtual void		critical_finalize		();
		virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}

		virtual bool		check_start_conditions	();
		virtual bool		check_completion		();

private:
				// выполнять состояние
				void		ExecuteGraviStart		();
				void		ExecuteGraviContinue	();
				void		ExecuteGraviFire		();

private:
	enum 
	{
		ACTION_GRAVI_STARTED,
		ACTION_GRAVI_CONTINUE,
		ACTION_GRAVI_FIRE,
		ACTION_WAIT_ANIM_END,
		ACTION_COMPLETED,

	}						m_action;

	u32						m_time_gravi_started;
	TTime					m_next_gravi_allowed_tick;
	TTime					m_anim_end_tick;
};

#include "burer_state_attack_gravi_inline.h"

