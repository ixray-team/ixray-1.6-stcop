#pragma once

#include "../state.h"
#include "../../../ai_debug.h"

class CStateMonsterRestFun : public CState {
	typedef CState inherited;

	u32					time_last_hit;

public:
						CStateMonsterRestFun	(CBaseMonster *obj);
	virtual	void		initialize				();
	virtual	void		execute					();
	virtual	bool		check_completion		();
	virtual	bool		check_start_conditions	();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};

#include "monster_state_rest_fun_inline.h"
