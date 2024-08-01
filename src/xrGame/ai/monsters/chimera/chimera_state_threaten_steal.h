#pragma once
#include "../state.h"

template<typename _Object>
class CStateChimeraThreatenSteal : public CStateMonsterMoveToPointEx<_Object> {
	typedef CStateMonsterMoveToPointEx<_Object>		inherited;

public:
						CStateChimeraThreatenSteal	(_Object *obj) : inherited(obj){}
						virtual ~CStateChimeraThreatenSteal() {}

	virtual	void		initialize					();	
	virtual void		finalize					();
	virtual	void		execute						();
	virtual bool		check_completion			();
	virtual bool		check_start_conditions		();
};

#include "chimera_state_threaten_steal_inline.h"
