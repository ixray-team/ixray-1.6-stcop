#pragma once
#include "../state.h"
#include "state_data.h"

template<typename _Object>
class CStateMonsterHideFromPoint : public CState<_Object> {
	typedef CState<_Object> inherited;

	SStateHideFromPoint data;

public:
						CStateMonsterHideFromPoint	(_Object *obj) : inherited(obj, &data){}
	virtual ~CStateMonsterHideFromPoint	() override = default;

	virtual void		initialize					() override;
	virtual void		execute						() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}

	virtual bool		check_completion			() override;
						
};

#include "state_hide_from_point_inline.h"