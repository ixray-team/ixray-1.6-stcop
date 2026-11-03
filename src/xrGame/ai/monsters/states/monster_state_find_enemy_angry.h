#pragma once

#include "../state.h"

template<typename _Object>
class CStateMonsterFindEnemyAngry : public CState<_Object> {
	typedef CState<_Object> inherited;

public:
						CStateMonsterFindEnemyAngry	(_Object *obj);
	virtual ~CStateMonsterFindEnemyAngry() override;

	virtual void		execute						() override;
	virtual bool		check_completion			() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}
};

#include "monster_state_find_enemy_angry_inline.h"
