#pragma once
#include "../state.h"
#include "../../../../xrServerEntities/clsid_game.h"

class	CustomBloodsuckerStateVampire : public CState
{
	using inherited = CState;
	using state_ptr = CState*;

	const CEntityAlive *enemy;
    CustomBloodsucker* m_pBloodsucker;

public:
	CustomBloodsuckerStateVampire(CustomBloodsucker*object);
	virtual ~CustomBloodsuckerStateVampire();

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
