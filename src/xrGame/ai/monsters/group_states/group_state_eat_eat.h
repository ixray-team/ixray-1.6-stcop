#pragma once
#include "../state.h"

class CStateGroupEating : public CState {
protected:
	typedef CState	inherited;

	CEntityAlive	*corpse;
	u32				time_last_eat;
	CustomDog* m_pDog;

public:
	CStateGroupEating		(CBaseMonster *obj);
	virtual				~CStateGroupEating	();

	virtual void		initialize				();
	virtual	void		execute					();

	virtual bool		check_start_conditions	();
	virtual bool		check_completion		();
	virtual void		remove_links			(CObject* object);
};
