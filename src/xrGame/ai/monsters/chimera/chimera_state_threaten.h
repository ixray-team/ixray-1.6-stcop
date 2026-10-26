#pragma once
#include "../state.h"

template<typename _Object>
class	CStateChimeraThreaten : public CState<_Object> {
protected:
	typedef CState<_Object> inherited;
	typedef CState<_Object>* state_ptr;

	enum {
		eStateWalk			= u32(0),
		eStateFaceEnemy,
		eStateThreaten,
		eStateSteal
	};

	u32					m_last_time_threaten;

public:
						CStateChimeraThreaten	(_Object *obj);
	virtual ~CStateChimeraThreaten	() override;

	virtual void		reinit					() override;

	virtual void		initialize				() override;

	virtual void		reselect_state			() override;
	virtual void 		finalize				() override;
	virtual void 		critical_finalize		() override;
	virtual bool 		check_start_conditions	() override;
	virtual bool 		check_completion		() override;
	virtual void		remove_links			(CObject* object) override { inherited::remove_links(object);}
};

#include "chimera_state_threaten_inline.h"
