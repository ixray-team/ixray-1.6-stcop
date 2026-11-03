#pragma once
#include "../state.h"

template<typename _Object>
class CStateMonsterEating : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;

	CEntityAlive	*corpse;
	u32				time_last_eat;

public:
						CStateMonsterEating		(_Object *obj);
	virtual ~CStateMonsterEating	() override;

	virtual void		initialize				() override;
	virtual void		execute					() override;
	virtual void		remove_links			(CObject* object) override;

	virtual bool		check_start_conditions	() override;
	virtual bool		check_completion		() override;
};

#include "monster_state_eat_eat_inline.h"
