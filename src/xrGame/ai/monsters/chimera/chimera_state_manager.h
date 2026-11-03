#pragma once
#include "../monster_state_manager.h"

class CChimera;

class CStateManagerChimera : public CMonsterStateManager<CChimera>
{
private:
	typedef				CMonsterStateManager<CChimera>	inherited;

public:
						CStateManagerChimera	(CChimera *obj);
	virtual ~CStateManagerChimera	() override;

	virtual void		execute					() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}
};
