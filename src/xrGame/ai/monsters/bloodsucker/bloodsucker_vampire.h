#pragma once
#include "../state.h"
#include "../../../../xrServerEntities/clsid_game.h"

class	CustomBloodsuckerStateVampire : public CState
{
protected:
	using inherited = CState;
	using state_ptr = CState*;

	const CEntityAlive *enemy;
    CBloodsuckerBase* pBloodsuckerBase;

public:
	CustomBloodsuckerStateVampire(CBloodsuckerBase*object);
	virtual ~CustomBloodsuckerStateVampire() override;

	virtual void		reinit							() override;
	
	virtual void		initialize						() override;
	virtual	void		reselect_state					() override;
	virtual	void		finalize						() override;
	virtual	void		critical_finalize				() override;
	virtual bool		check_start_conditions			() override;
	virtual bool		check_completion				() override;
	virtual void		remove_links					(CObject* object) override;

	virtual void		setup_substates					() override;
	virtual void		check_force_state				() override;
};
