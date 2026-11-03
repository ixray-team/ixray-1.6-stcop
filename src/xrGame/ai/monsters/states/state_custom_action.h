#pragma once
#include "../state.h"
#include "state_data.h"

template<typename _Object>
class CStateMonsterCustomAction : public CState<_Object> {
	typedef CState<_Object> inherited;

	SStateDataAction	data;

public:
						CStateMonsterCustomAction	(_Object *obj);
	virtual ~CStateMonsterCustomAction	() override;

	virtual void		execute						() override;
	virtual bool		check_completion			() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}
};

#include "state_custom_action_inline.h"