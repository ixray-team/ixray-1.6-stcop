#pragma once
#include "../state.h"

#include "../../../alife_smart_terrain_task.h"

class CStateMonsterSmartTerrainTask : public CState{
	typedef CState		inherited;
	typedef CState*	state_ptr;

	CALifeSmartTerrainTask *m_current_task;
public:
						CStateMonsterSmartTerrainTask	(CBaseMonster*obj);
	virtual				~CStateMonsterSmartTerrainTask	();

	virtual void		initialize				();
	virtual	void		reselect_state			();
	virtual bool		check_start_conditions	();
	virtual bool		check_completion		();
	virtual void		setup_substates			();
	virtual void		check_force_state		();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
