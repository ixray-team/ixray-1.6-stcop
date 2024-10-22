#pragma once

#include "../state.h"

class	CStateControllerFastMove : public CState {
protected:
	using inherited = CState;
	CControllerBase* pControllerBase;

public:
	CStateControllerFastMove(CBaseMonster* object);
	virtual ~CStateControllerFastMove() override;

	virtual void		initialize					() override;
	virtual void		finalize					() override;
	virtual void		critical_finalize			() override;

	virtual void		execute						() override;
};
