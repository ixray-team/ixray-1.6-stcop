#pragma once
#include "../state.h"

class CStateControllerTube : public CState {
protected:
	using inherited = CState	;
	CControllerBase* pControllerBase;

public:
	CStateControllerTube(CBaseMonster* object);
	virtual ~CStateControllerTube() override;

	virtual void		execute					() override;
	virtual bool		check_start_conditions	() override;
	virtual bool		check_completion		() override;
};
