#pragma once

#include "../state.h"
#include "../../../ai_debug.h"

class CStateMonsterRestSleep : public CState {
	typedef CState inherited;
public:
						CStateMonsterRestSleep	(CBaseMonster *obj);
	virtual				~CStateMonsterRestSleep	();

	virtual	void		initialize				();
	virtual	void		execute					();
	virtual	void		finalize				();
	virtual	void		critical_finalize		();
	virtual void		remove_links			(CObject* object_) { inherited::remove_links(object_);}
};
