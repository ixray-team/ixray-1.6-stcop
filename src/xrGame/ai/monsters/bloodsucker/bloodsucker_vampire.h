#pragma once
#include "../state.h"
#include "../../../../xrServerEntities/clsid_game.h"

class	CStateBloodsuckerVampire : public CState {
	typedef CState		inherited;
	typedef CState*	state_ptr;

	const CEntityAlive *enemy;

public:
						CStateBloodsuckerVampire		(CAI_Bloodsucker*obj);
						CAI_Bloodsucker* object;
	virtual void		reinit							();
	
	virtual void		initialize						();
	virtual	void		reselect_state					();
	virtual	void		finalize						();
	virtual	void		critical_finalize				();
	virtual bool		check_start_conditions			();
	virtual bool		check_completion				();
	virtual void		remove_links					(CObject* object);

	virtual void		setup_substates					();
	virtual void		check_force_state				();
};

#include "bloodsucker_vampire_inline.h"
