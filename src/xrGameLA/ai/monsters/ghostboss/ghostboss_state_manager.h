#pragma once
#include "../monster_state_manager.h"

class CGhostBoss;

class CStateManagerGhostBoss : public CMonsterStateManager<CGhostBoss> {
	typedef CMonsterStateManager<CGhostBoss> inherited;
public:
					CStateManagerGhostBoss		(CGhostBoss *monster); 
	virtual void	execute					();
	virtual void	setup_substates			();

private:
	float				m_last_health;
};

