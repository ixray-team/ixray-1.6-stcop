#pragma once
#include "../state.h"

class CStateChimeraThreatenSteal : public CStateMonsterMoveToPointEx {
protected:
	using inherited = CStateMonsterMoveToPointEx;

public:
	CStateChimeraThreatenSteal(CBaseMonster* object);
	virtual ~CStateChimeraThreatenSteal() override;

	virtual	void		initialize					() override;
	virtual void		finalize					() override;
	virtual	void		execute						() override;
	virtual bool		check_completion			() override;
	virtual bool		check_start_conditions		() override;
};
