#pragma once

#include "../state.h"
#include "../../../ai_debug.h"

template<typename _Object>
class	CStateGroupAttack : public CState<_Object> 
{
public:
	CStateGroupAttack (_Object *obj);
	virtual ~CStateGroupAttack	() override;

	virtual void		initialize			() override;
	virtual void		execute				() override;
	virtual void		setup_substates		() override;
	virtual void		critical_finalize	() override;
	virtual void		finalize		    () override;
	virtual void		remove_links		(CObject* object) override;

protected:
	typedef CState<_Object>		inherited;
	typedef CState<_Object>*	state_ptr;

	const CEntityAlive* m_enemy;
	u32					m_time_next_run_away;
	u32					m_time_start_check_behinder;
	u32					m_time_start_behinder;
	float				m_delta_distance;
	u32					m_time_start_drive_out;
	bool				m_drive_out;

protected:
	bool				check_home_point	();
	bool				check_behinder		();
};

#include "group_state_attack_inline.h"
