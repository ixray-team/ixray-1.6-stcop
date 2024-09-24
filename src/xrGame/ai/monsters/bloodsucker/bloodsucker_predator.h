#pragma once
#include "../state.h"

class	CustomBloodsuckerStatePredator : public CState 
{
	using inherited = CState;
	using state_ptr = CState*;

	u32							m_target_node;
	u32							m_time_start_camp;
	CustomBloodsucker*			m_pBloodsucker;

public:
	CustomBloodsuckerStatePredator(CustomBloodsucker* object);
	virtual ~CustomBloodsuckerStatePredator();

	virtual void		reinit							();

	virtual void		initialize						();
	virtual	void		reselect_state					();
	virtual	void		finalize						();
	virtual	void		critical_finalize				();
	virtual bool		check_start_conditions			();
	virtual bool		check_completion				();
	virtual void		remove_links					(CObject* object) { inherited::remove_links(object);}

	virtual void		setup_substates					();
	virtual void		check_force_state				();

private:
			void		select_camp_point				();
};
