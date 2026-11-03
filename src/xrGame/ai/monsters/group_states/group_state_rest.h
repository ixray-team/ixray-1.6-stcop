#pragma once

#include "../state.h"
#include "../../../EntityCondition.h"
#include "../states/state_data.h"
template<typename _Object>
class	CStateGroupRest : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	u32					time_for_life;
	u32					time_for_sleep;

public:
						CStateGroupRest		(_Object *obj);
	virtual ~CStateGroupRest		() override;

	virtual void		initialize				() override;
	virtual void		execute					() override;
	virtual void		finalize				() override;
	virtual void		critical_finalize		() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}
};

#include "group_state_rest_inline.h"