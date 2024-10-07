#pragma once

#include "../state.h"

class	CStateControllerFastMove : public CState {
protected:
	typedef CState		inherited;
	CControllerBase* m_pController;

public:
	CStateControllerFastMove(CBaseMonster* obj);
	virtual void		initialize					();	
	virtual void		finalize					();	
	virtual void		critical_finalize			();

	virtual void		execute						();
};
