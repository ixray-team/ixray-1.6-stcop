#pragma once
#include "../state.h"

class CStateMonsterEating : public CState {
protected:
	typedef CState		inherited;

	CEntityAlive	*corpse;
	u32				time_last_eat;

public:
						CStateMonsterEating		(CBaseMonster *obj);
	virtual				~CStateMonsterEating	();

	virtual void		initialize				();
	virtual	void		execute					();
	virtual void		remove_links			(CObject* object);

	virtual bool		check_start_conditions	();
	virtual bool		check_completion		();
};
