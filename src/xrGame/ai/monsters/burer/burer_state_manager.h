#pragma once
#include "../monster_state_manager.h"

class CBurerBase;

class CStateManagerBurer : public CMonsterStateManager {
	typedef CMonsterStateManager inherited;

	CBurerBase* m_pBurer;

public:
					CStateManagerBurer		(CBurerBase *monster); 
	virtual void	execute					();
	virtual void	setup_substates			();
	virtual void	remove_links			(CObject* object_) { inherited::remove_links(object_);}
};

