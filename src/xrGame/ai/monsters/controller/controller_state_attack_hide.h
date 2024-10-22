#pragma once

#include "../state.h"

class CStateControlHide : public CState {
protected:
	using	inherited = CState;
	using	state_ptr = CState*;
	CControllerBase* pControllerBase;
	bool			m_cover_reached;

	struct {
		Fvector position;
		u32		node;
	} target;

	u32				m_time_finished;

	bool			m_state_fast_run;

public:

	CStateControlHide(CBaseMonster* object);
	virtual			~CStateControlHide() override;

	virtual void	initialize				() override;
	virtual void	execute					() override;

	virtual void	finalize				() override;
	virtual void	critical_finalize		() override;
	virtual void	remove_links			(CObject* object) override { inherited::remove_links(object);}

	virtual bool 	check_completion		() override;
	virtual bool 	check_start_conditions	() override;

private:
			void	select_target_point		();
};

