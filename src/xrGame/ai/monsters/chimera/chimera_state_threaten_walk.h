#pragma once
#include "../state.h"

class CStateChimeraThreatenWalk : public CStateMonsterMoveToPointEx {
protected:
	using inherited = CStateMonsterMoveToPointEx	;

public:
	CStateChimeraThreatenWalk(CBaseMonster* object);
	virtual ~CStateChimeraThreatenWalk() override;

	virtual	void		initialize					() override;
	virtual	void		execute						() override;
	virtual bool		check_completion			() override;
	virtual bool		check_start_conditions		() override;
};
