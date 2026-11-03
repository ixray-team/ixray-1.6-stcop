#pragma once
#include "../state.h"

template<typename _Object>
class CStateMonsterSmartTerrainTaskGraphWalk : public CStateMove<_Object> {
	typedef CStateMove<_Object> inherited;

	CALifeSmartTerrainTask	*m_task;

public:
						CStateMonsterSmartTerrainTaskGraphWalk	(_Object *obj) : inherited(obj) {}
	virtual void		initialize				() override;
	virtual void		execute					() override;
	virtual bool		check_start_conditions	() override;
	virtual bool		check_completion		() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}
};

#include "monster_state_smart_terrain_task_graph_walk_inline.h"