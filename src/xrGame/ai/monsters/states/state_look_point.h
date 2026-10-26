#pragma once
#include "../state.h"
#include "state_data.h"

template<typename _Object>
class CStateMonsterLookToPoint : public CState<_Object> {
	typedef CState<_Object> inherited;

	SStateDataLookToPoint	data;

public:
						CStateMonsterLookToPoint	(_Object *obj);
	virtual ~CStateMonsterLookToPoint	() override;

	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}

	virtual bool		check_completion			() override;
};

#include "state_look_point_inline.h"