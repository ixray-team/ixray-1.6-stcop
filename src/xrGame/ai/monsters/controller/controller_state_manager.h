#pragma once
#include "../monster_state_manager.h"

class CController;

class CStateManagerController : public CMonsterStateManager<CController> {

	typedef CMonsterStateManager<CController> inherited;

public:
						CStateManagerController			(CController *obj);
	virtual ~CStateManagerController		() override;

	virtual void		reinit							() override;
	virtual void		execute							() override;
	virtual void		remove_links					(CObject* object_) override { inherited::remove_links(object_);}
	virtual bool		check_control_start_conditions	(ControlCom::EControlType type) override;
};
