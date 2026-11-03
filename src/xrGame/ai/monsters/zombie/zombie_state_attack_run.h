#pragma once

#include "../state.h"

template<typename _Object>
class CStateZombieAttackRun : public CState<_Object> {
	typedef CState<_Object> inherited;

	TTime				m_time_action_change;
	EAction				action;

public:
						CStateZombieAttackRun	(_Object *obj);
	virtual ~CStateZombieAttackRun	() override;

	virtual void		initialize				() override;
	virtual void		execute					() override;

	virtual bool 		check_completion		() override;
	virtual bool 		check_start_conditions	() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}

private:
			void		choose_action			();

};

#include "zombie_state_attack_run_inline.h"
