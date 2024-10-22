#pragma once

#include "../state.h"

// Hiding until enemy get out from its sight
class CStateControlHideLite : public CState {
protected:
	using	inherited = CState	;
	using	state_ptr = CState*;

	struct {
		Fvector position;
		u32		node;
	} target;
	CControllerBase* pControllerBase;
	u32				m_time_finished;

public:
	CStateControlHideLite(CBaseMonster* object);
	virtual			~CStateControlHideLite() override;
	
	virtual void	reinit					() override;

	virtual void	initialize				() override;
	virtual void	execute					() override;

	virtual void	finalize				() override;

	virtual bool 	check_completion		() override;
	virtual bool 	check_start_conditions	() override;
	virtual void	remove_links(CObject* object) override { inherited::remove_links(object); }

private:
	void	select_target_point		();
};
