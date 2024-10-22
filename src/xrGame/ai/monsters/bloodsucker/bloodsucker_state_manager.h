#pragma once
#include "../monster_state_manager.h"

class CustomBloodsuker;

class CBloodsuckerBaseStateManager : public CMonsterStateManager
{
protected:
	using inherited = CMonsterStateManager;

	CBloodsuckerBase* pBloodsuckerBase;

public:
	CBloodsuckerBaseStateManager(CBloodsuckerBase* object);
	virtual ~CBloodsuckerBaseStateManager() override;

	virtual void	execute						() override;
	virtual void	update						() override;
			void	drag_object					();
	virtual void	remove_links				(CObject* object) override { inherited::remove_links(object);}
			bool	check_vampire				();
};
