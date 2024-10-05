#pragma once
#include "../state.h"

class CStateMonsterHitObject : public CState {
	typedef CState inherited;

	xr_vector<CObject*>	m_nearest_objects;
	CPhysicsShellHolder	*target;
	bool				m_hitted;
		
public:
					CStateMonsterHitObject	(CBaseMonster*obj) : inherited(obj) {}
	
	virtual	void	initialize				();
	virtual	void	execute					();
	virtual bool	check_start_conditions	();
	virtual bool	check_completion		();
};
