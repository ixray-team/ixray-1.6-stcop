#pragma once
#include "../state.h"

class	CStateChimeraThreaten : public CState {
protected:
	typedef CState inherited;
	typedef CState * state_ptr;

	enum {
		eStateWalk			= u32(0),
		eStateFaceEnemy,
		eStateThreaten,
		eStateSteal
	};

	u32					m_last_time_threaten;

public:
						CStateChimeraThreaten	(CBaseMonster *obj);
	virtual				~CStateChimeraThreaten	();

	virtual void		reinit					();

	virtual	void		initialize				();

	virtual	void		reselect_state			();
	virtual void 		finalize				();
	virtual void 		critical_finalize		();
	virtual bool 		check_start_conditions	();	
	virtual bool 		check_completion		();	
	virtual void		remove_links			(CObject* object) { inherited::remove_links(object);}
};

#include "chimera_state_threaten_inline.h"
