#pragma once
#include "../monster_state_manager.h"

class CAI_SwampBeast;

class CStateManagerSwampBeast : public CMonsterStateManager<CAI_SwampBeast> {
	typedef CMonsterStateManager<CAI_SwampBeast> inherited;

public:

					CStateManagerSwampBeast	(CAI_SwampBeast *monster); 
	virtual void	execute					();
};
