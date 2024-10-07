#pragma once
#include "../state.h"

class CStateGroupEating : public CState {
protected:
	using inherited = CState	;

	CEntityAlive	*corpse;
	u32				time_last_eat;

	CDogBase* m_pDog;

public:
	CStateGroupEating		(CBaseMonster * object);
	virtual				~CStateGroupEating	();

	virtual void		initialize				();
	virtual	void		execute					();

	virtual bool		check_start_conditions	();
	virtual bool		check_completion		();
	virtual void		remove_links			(CObject* object);
};
