#pragma once
#include "../state.h"

class	CStateBloodsuckerPredatorLite : public CState {
	typedef CState	inherited;
	typedef CState*	state_ptr;

	u32							m_target_node;
	bool						m_freezed;

public:
						CStateBloodsuckerPredatorLite		(CAI_Bloodsucker*obj);

	virtual void		reinit							();

	virtual void		initialize						();
	virtual	void		reselect_state					();
	virtual	void		finalize						();
	virtual	void		critical_finalize				();
	virtual bool		check_completion				();

	virtual void		setup_substates					();
	virtual void		check_force_state				();

private:
			void		select_camp_point				();
			bool		enemy_see_me					();

};

#include "bloodsucker_predator_lite_inline.h"