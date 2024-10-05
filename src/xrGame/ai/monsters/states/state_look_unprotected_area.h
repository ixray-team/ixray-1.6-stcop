#pragma once
#include "../state.h"
#include "state_data.h"
#include "../../../ai_object_location.h"
#include "../../../ai_space.h"
#include "../../../level_graph.h"

class CStateMonsterLookToUnprotectedArea : public CState {
	typedef CState inherited;

	SStateDataAction	data;

	Fvector				target_point;	

public:
						CStateMonsterLookToUnprotectedArea	(CBaseMonster*obj);
	virtual				~CStateMonsterLookToUnprotectedArea	();

	virtual void		initialize					();
	virtual	void		execute						();
	virtual void		remove_links				(CObject* object_) { inherited::remove_links(object_);}

	virtual bool		check_completion			();
};
