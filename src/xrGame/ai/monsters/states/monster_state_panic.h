#pragma once

#include "../state.h"

template<typename _Object>
class	CStateMonsterPanic : public CState<_Object> {
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;
	
public:
						CStateMonsterPanic		(_Object *obj);
	virtual ~CStateMonsterPanic		() override;

	virtual void		initialize				() override;
	virtual void		reselect_state			() override;
	virtual void		check_force_state		() override;
	virtual void		setup_substates			() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}
};

#include "monster_state_panic_inline.h"
