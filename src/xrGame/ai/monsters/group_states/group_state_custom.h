#pragma once

#include "../state.h"

template<typename _Object>
class	CStateCustomGroup : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

public:
	CStateCustomGroup		(_Object *obj);
	virtual ~CStateCustomGroup		() override;

	virtual void		execute					() override;
	virtual void		setup_substates			() override;
	virtual bool 		check_completion		() override {return (this->object->b_state_end);}
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}
};

#include "group_state_custom_inline.h"