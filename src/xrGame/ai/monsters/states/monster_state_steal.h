#pragma once
#include "../state.h"

template<typename _Object>
class CStateMonsterSteal : public CState<_Object> {
	typedef CState<_Object> inherited;

public:
						CStateMonsterSteal		(_Object *obj);

	virtual void		initialize				() override;
	virtual void		execute					() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}

	virtual bool 		check_completion		() override;
	virtual bool 		check_start_conditions	() override;

private:
			bool		check_conditions		();
};

#include "monster_state_steal_inline.h"
