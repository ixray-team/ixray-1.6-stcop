#pragma once

#include "../state.h"
#include "../../../ai_debug.h"

template<typename _Object>
class CStateMonsterRestSleep : public CState<_Object> {
	typedef CState<_Object> inherited;
public:
						CStateMonsterRestSleep	(_Object *obj);
	virtual ~CStateMonsterRestSleep	() override;

	virtual void		initialize				() override;
	virtual void		execute					() override;
	virtual void		finalize				() override;
	virtual void		critical_finalize		() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}
};

#include "monster_state_rest_sleep_inline.h"
