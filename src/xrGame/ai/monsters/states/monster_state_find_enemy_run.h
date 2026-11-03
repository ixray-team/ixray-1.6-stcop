#pragma once

#include "../state.h"

template<typename _Object>
class CStateMonsterFindEnemyRun : public CState<_Object> {
	typedef CState<_Object> inherited;

	Fvector				target_point;
	u32					target_vertex;

public:
						CStateMonsterFindEnemyRun	(_Object *obj);
	virtual ~CStateMonsterFindEnemyRun	() override;

	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual bool		check_completion			() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}
};

#include "monster_state_find_enemy_run_inline.h"
