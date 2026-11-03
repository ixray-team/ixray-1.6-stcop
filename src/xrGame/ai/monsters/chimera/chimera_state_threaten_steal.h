#pragma once
#include "../state.h"

template<typename _Object>
class CStateChimeraThreatenSteal : public CStateMonsterMoveToPointEx<_Object> {
	typedef CStateMonsterMoveToPointEx<_Object>		inherited;

public:
	IC					CStateChimeraThreatenSteal	(_Object *obj) : inherited(obj){}
	virtual void		initialize					() override;
	virtual void		finalize					() override;
	virtual void		execute						() override;
	virtual bool		check_completion			() override;
	virtual bool		check_start_conditions		() override;
};

#include "chimera_state_threaten_steal_inline.h"
