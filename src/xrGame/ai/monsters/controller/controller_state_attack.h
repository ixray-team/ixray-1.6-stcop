#pragma once

#include "../state.h"
#include "../../../ai_debug.h"

template<typename _Object>
class	CStateControllerAttack : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

public:
						CStateControllerAttack	(_Object *obj);
	virtual ~CStateControllerAttack	() override = default;

	virtual void		initialize				() override;
	virtual void		finalize				() override;
	virtual void		critical_finalize		() override;

	virtual void		execute					() override;
	virtual void		setup_substates			() override;
	virtual void		check_force_state		() override;
	virtual void		remove_links			(CObject * ) override {}
			bool		check_home_point		();
};

#include "controller_state_attack_inline.h"
