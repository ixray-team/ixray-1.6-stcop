#pragma once
#include "../state.h"

template<typename _Object>
class CStateMonsterTestState : public CState<_Object> {
	typedef CState<_Object> inherited;
	typedef CState<_Object> *state_ptr;

public:
						CStateMonsterTestState	(_Object *obj);
	virtual void		reselect_state			() override;
	virtual void		setup_substates			() override;
	virtual void		remove_links			(CObject* object) override { inherited::remove_links(object);}
};

template<typename _Object>
class CStateMonsterTestCover : public CState<_Object> {
	typedef CState<_Object> inherited;
	typedef CState<_Object> *state_ptr;

	u32					m_last_node;

public:
						CStateMonsterTestCover	(_Object *obj);
	virtual void		initialize				() override;
	virtual void		check_force_state		() override;
	virtual void		reselect_state			() override;
	virtual void		setup_substates			() override;
	virtual void		remove_links			(CObject* object_) override { inherited::remove_links(object_);}
};

#include "state_test_state_inline.h"