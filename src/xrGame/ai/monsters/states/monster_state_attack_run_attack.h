#pragma once
#include "../state.h"

template<typename _Object>
class CStateMonsterAttackRunAttack : public CState<_Object> {
	typedef CState<_Object> inherited;

public:
						CStateMonsterAttackRunAttack	(_Object *obj) : inherited(obj) {};

	virtual void		initialize						() override;
	virtual void		execute							() override;
	virtual void		finalize						() override;
	virtual void		critical_finalize				() override;
	virtual void		remove_links					(CObject* object_) override { inherited::remove_links(object_);}

	virtual bool 		check_completion				() override;
	virtual bool 		check_start_conditions			() override;
};

#include "monster_state_attack_run_attack_inline.h"
