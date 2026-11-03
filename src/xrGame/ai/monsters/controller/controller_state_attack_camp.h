#pragma once

template<typename _Object>
class CStateControlCamp : public CState<_Object> {
	typedef	CState<_Object>		inherited;

	float			m_angle_from;
	float			m_angle_to;
	
	float			m_target_angle;
	u32				m_time_next_updated;

public:

					CStateControlCamp	(_Object *obj) : inherited(obj) {}
	virtual ~CStateControlCamp	() override = default;

	virtual void	initialize				() override;
	virtual void	execute					() override;
	virtual bool	check_completion		() override;
	virtual bool	check_start_conditions	() override;

	virtual void	remove_links			(CObject* object) override {}

private:

	virtual void	update_target_angle		();

};

#include "controller_state_attack_camp_inline.h"
