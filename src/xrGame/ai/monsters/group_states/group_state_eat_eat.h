#pragma once
#include "../state.h"

template<typename _Object>
class CStateGroupEating : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;

	CEntityAlive	*corpse;
	u32				time_last_eat;

public:
	CStateGroupEating		(_Object *obj);
	virtual ~CStateGroupEating	() override;

	virtual void		initialize				() override;
	virtual void		execute					() override;

	virtual bool		check_start_conditions	() override;
	virtual bool		check_completion		() override;
	virtual void		remove_links			(CObject* object) override;
};

#include "group_state_eat_eat_inline.h"