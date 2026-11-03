#pragma once
#include "state_manager.h"
#include "state.h"

// Lain: added
#ifdef DEBUG
#include "debug_text_tree.h"
#endif

template <typename _Object>
class CMonsterStateManager : 
	public IStateManagerBase, 
	public CState<_Object> 
{
	typedef CState<_Object> inherited;

public:
					CMonsterStateManager	(_Object *obj) : inherited(obj) {}
	virtual void	reinit					() override;
	virtual void	update					() override;
	virtual void	force_script_state		(EMonsterState state) override;
	virtual void	execute_script_state	() override;
	virtual void	critical_finalize		() override;
	virtual void	remove_links			(CObject* object_) override { inherited::remove_links(object_); }

	virtual EMonsterState get_state_type	() override;

	virtual bool	check_control_start_conditions (ControlCom::EControlType type) override
	{ return inherited::check_control_start_conditions(type); }

	// Lain: added
	#ifdef DEBUG
	virtual void    add_debug_info          (debug::text_tree& root_s) override;
	#endif

protected:
			bool	can_eat					();
			bool	check_state				(u32 state_id);
};

#include "monster_state_manager_inline.h"