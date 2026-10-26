#pragma once
#include "../state.h"
#include "../../../../xrServerEntities/clsid_game.h"

template<typename _Object>
class	CStateBloodsuckerVampire : public CState<_Object> {
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	const CEntityAlive *enemy;

public:
						CStateBloodsuckerVampire		(_Object *obj);

	virtual void		reinit							() override;

	virtual void		initialize						() override;
	virtual void		reselect_state					() override;
	virtual void		finalize						() override;
	virtual void		critical_finalize				() override;
	virtual bool		check_start_conditions			() override;
	virtual bool		check_completion				() override;
	virtual void		remove_links					(CObject* object) override;

	virtual void		setup_substates					() override;
	virtual void		check_force_state				() override;
};

#include "bloodsucker_vampire_inline.h"
