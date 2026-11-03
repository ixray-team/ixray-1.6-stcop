#pragma once
#include "../state.h"

#include "../../../alife_smart_terrain_task.h"

template<typename _Object>
class CStateMonsterSmartTerrainTask : public CState<_Object> {
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	CALifeSmartTerrainTask *m_current_task;
public:
						CStateMonsterSmartTerrainTask	(_Object *obj);
	virtual ~CStateMonsterSmartTerrainTask	() override;

	virtual void		initialize				() override;
	virtual void		reselect_state			() override;
	virtual bool		check_start_conditions	() override;
	virtual bool		check_completion		() override;
	virtual void		setup_substates			() override;
	virtual void		check_force_state		() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}
};

#include "monster_state_smart_terrain_task_inline.h"
