#pragma once
#include "../state.h"

template<typename _Object>
class CStateControllerTube : public CState<_Object> {
	typedef CState<_Object>		inherited;

public:
						CStateControllerTube	(_Object *obj) : inherited(obj){}
	virtual void		execute					() override;
	virtual bool		check_start_conditions	() override;
	virtual bool		check_completion		() override;
};

#include "controller_tube_inline.h"

