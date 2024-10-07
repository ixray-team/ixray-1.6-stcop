#pragma once
#include "../state.h"

class CStateControllerTube : public CState {
	typedef CState		inherited;
	CControllerBase* m_pController;
public:
	CStateControllerTube(CBaseMonster* obj);
	virtual void		execute					();
	virtual bool		check_start_conditions	();
	virtual bool		check_completion		();
};
