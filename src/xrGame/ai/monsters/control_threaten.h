#pragma once
#include "control_combase.h"

struct SControlThreatenData : public ControlCom::IComData {
	const char*	animation;
	float	time;
};

class CControlThreaten : public CControl_ComCustom<SControlThreatenData> {
	typedef CControl_ComCustom<SControlThreatenData> inherited;

public:
	virtual void	reinit					() override;
	virtual void	update_schedule			() override;
	virtual void	on_event				(ControlCom::EEventType, ControlCom::IEventData*) override;
	virtual void	activate				() override;
	virtual void	on_release				() override;
	virtual bool	check_start_conditions	() override;
};

