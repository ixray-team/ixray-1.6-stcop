#pragma once

// Hiding until enemy get out from its sight
template<typename _Object>
class CStateControlHideLite : public CState<_Object> {
	typedef	CState<_Object>		inherited;
	typedef	CState<_Object>*	state_ptr;

	struct {
		Fvector position;
		u32		node;
	} target;

	u32				m_time_finished;

public:

					CStateControlHideLite	(_Object *obj) : inherited(obj) {}
	virtual ~CStateControlHideLite	() override = default;

	virtual void	reinit					() override;

	virtual void	initialize				() override;
	virtual void	execute					() override;

	virtual void	finalize				() override;

	virtual bool 	check_completion		() override;
	virtual bool 	check_start_conditions	() override;
	virtual void	remove_links			(CObject* object) override {}


private:
	void	select_target_point		();
};


#include "controller_state_attack_hide_lite_inline.h"

