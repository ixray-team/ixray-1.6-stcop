#pragma once
#include "../state.h"

template<typename _Object>
class	CStateBloodsuckerPredator : public CState<_Object> {
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	u32							m_target_node;
	u32							m_time_start_camp;

public:
						CStateBloodsuckerPredator		(_Object *obj);

	virtual void		reinit							() override;

	virtual void		initialize						() override;
	virtual void		reselect_state					() override;
	virtual void		finalize						() override;
	virtual void		critical_finalize				() override;
	virtual bool		check_start_conditions			() override;
	virtual bool		check_completion				() override;
	virtual void		remove_links					(CObject* object) override { inherited::remove_links(object);}

	virtual void		setup_substates					() override;
	virtual void		check_force_state				() override;

private:
			void		select_camp_point				();
};

#include "bloodsucker_predator_inline.h"