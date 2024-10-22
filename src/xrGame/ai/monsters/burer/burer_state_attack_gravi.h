#pragma once
#include "../state.h"

class CStateBurerAttackGravi : public CState {
protected:
	using inherited = CState;

	CBurerBase* pBurerBase;

public:
							CStateBurerAttackGravi	(CBaseMonster* object);
							virtual ~CStateBurerAttackGravi() override;

		virtual	void		initialize				() override;
		virtual	void		execute					() override;
		virtual void		finalize				() override;
		virtual void		critical_finalize		() override;
		virtual void		remove_links			(CObject* object) override { inherited::remove_links(object);}

		virtual bool		check_start_conditions	() override;
		virtual bool		check_completion		() override;

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
