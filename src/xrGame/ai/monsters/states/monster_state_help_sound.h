#pragma once

#include "../state.h"

template<typename _Object>
class	CStateMonsterHearHelpSound : public CState<_Object> {
protected:
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

public:
					CStateMonsterHearHelpSound	(_Object *obj);
	virtual ~CStateMonsterHearHelpSound	() override = default;

	virtual void	reselect_state				() override;
	virtual void	setup_substates				() override;

	virtual bool	check_start_conditions		() override;
	virtual bool	check_completion			() override;

	virtual void	remove_links				(CObject* object_) override { inherited::remove_links(object_);}
};

#include "monster_state_help_sound_inline.h"