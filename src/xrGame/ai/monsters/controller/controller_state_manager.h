#pragma once
#include "../monster_state_manager.h"

class CControllerBase;

class CControllerBaseStateManager : public CMonsterStateManager {
protected:
	using inherited = CMonsterStateManager;
	CControllerBase* pControllerBase;

public:
	CControllerBaseStateManager(CControllerBase* object);
	virtual				~CControllerBaseStateManager() override;

	virtual void		reinit							() override;
	virtual	void		execute							() override;
	virtual void		remove_links					(CObject* object) override { inherited::remove_links(object);}
	virtual bool		check_control_start_conditions	(ControlCom::EControlType type) override;
};
