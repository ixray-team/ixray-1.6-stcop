#pragma once

#include "../state.h"

template<typename _Object>
class	CStateGroupEat : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	const CEntityAlive			*corpse;

	u32							m_time_last_eat;

public:
	CStateGroupEat		(_Object *obj);
	virtual ~CStateGroupEat		() override;

	virtual void		reinit					() override;
	virtual void		initialize				() override;
	virtual void		finalize				() override;
	virtual void		critical_finalize		() override;
	virtual void		remove_links			(CObject* object) override;

	virtual void		reselect_state			() override;
	virtual void		setup_substates			() override;
	virtual bool		check_completion		() override;
	virtual bool		check_start_conditions	() override;

private:

			bool		hungry					();
};

#include "group_state_eat_inline.h"