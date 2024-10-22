#pragma once
#include "../pseudodog/pseudodog_state_manager.h"

class CPseudoPsyDogBaseStateManager : public CPseudoDogBaseStateManager
{
protected:
	using inherited = CPseudoDogBaseStateManager;

public:
	CPseudoPsyDogBaseStateManager(CPseudoDogBase* object);
					virtual ~CPseudoPsyDogBaseStateManager() override;

	virtual void	execute				() override;
	virtual void	remove_links		(CObject* object) override { inherited::remove_links(object);}
};
