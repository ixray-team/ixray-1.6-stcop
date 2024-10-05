#pragma once

#include "../state.h"

class CStateMonsterAttackRun : public CState {
	typedef CState inherited;

	TTime				m_time_path_rebuild;

public:
	IC					CStateMonsterAttackRun	(CBaseMonster *obj) : inherited(obj) {}

	virtual void		initialize				();
	virtual	void		execute					();
	virtual void		finalize				();
	virtual void		critical_finalize		();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}

	virtual bool 		check_completion		();
	virtual bool 		check_start_conditions	();

};
