#pragma once

template<typename _Object>
class CStateMonsterPanicRun : public CState<_Object> {
	typedef CState<_Object> inherited;

public:
						CStateMonsterPanicRun	(_Object *obj) : inherited(obj) {}
	virtual ~CStateMonsterPanicRun	() override = default;

	virtual void		initialize				() override;
	virtual void		execute					() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}

	virtual bool		check_completion		() override;
};

#include "monster_state_panic_run_inline.h"
