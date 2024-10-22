#pragma once

#include "../state.h"
#include "../../../ai_debug.h"

class	CStateControllerAttack : public CState {
protected:
	using inherited = CState		;
	using state_ptr = CState*	;

public:
						CStateControllerAttack	(CBaseMonster *object);
						virtual				~CStateControllerAttack() override;

	virtual void		initialize				() override;
	virtual void		finalize				() override;
	virtual void		critical_finalize		() override;
	
	virtual void		execute					() override;
	virtual void		setup_substates			() override;
	virtual void		check_force_state		() override;
	virtual void		remove_links(CObject* object) override { inherited::remove_links(object); }

			bool		check_home_point		();
};
