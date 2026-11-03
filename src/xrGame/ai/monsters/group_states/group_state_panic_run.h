#pragma once

template<typename _Object>
class CStateGroupPanicRun : public CState<_Object> {
	typedef CState<_Object> inherited;

public:
						CStateGroupPanicRun	(_Object *obj) : inherited(obj) {}
	virtual ~CStateGroupPanicRun	() override = default;

	virtual void		initialize				() override;
	virtual void		execute					() override;

	virtual bool		check_completion		() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}
};

#include "group_state_panic_run_inline.h"
