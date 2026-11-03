#pragma once
#include "../state.h"

template<typename _Object>
class CStateMonsterMoveToRestrictor : public CState<_Object> {
	typedef CState<_Object> inherited;

public:
						CStateMonsterMoveToRestrictor	(_Object *obj) : inherited(obj) {}
	virtual ~CStateMonsterMoveToRestrictor	() override = default;

	virtual void		initialize					() override;
	virtual void		execute						() override;

	virtual bool		check_start_conditions		() override;
	virtual bool		check_completion			() override;
	virtual void		remove_links				(CObject* object_) override { inherited::remove_links(object_);}
};

#include "state_move_to_restrictor_inline.h"
