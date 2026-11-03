#pragma once

#include "../state.h"

template<typename _Object>
class CStateMonsterAttackRun : public CState<_Object> {
	typedef CState<_Object> inherited;

	TTime				m_time_path_rebuild;

public:
	IC					CStateMonsterAttackRun	(_Object *obj) : inherited(obj) {}

	virtual void		initialize				() override;
	virtual void		execute					() override;
	virtual void		finalize				() override;
	virtual void		critical_finalize		() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}

	virtual bool 		check_completion		() override;
	virtual bool 		check_start_conditions	() override;

};

#include "monster_state_attack_run_inline.h"
