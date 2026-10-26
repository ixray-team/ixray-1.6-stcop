#pragma once
#include "control_combase.h"
#include "../../../Include/xrRender/KinematicsAnimated.h"

struct SControlMeleeJumpData : public ControlCom::IComData {
	MotionID		anim_ls;
	MotionID		anim_rs;
};

class CControlMeleeJump : public CControl_ComCustom<SControlMeleeJumpData> {
	typedef CControl_ComCustom<SControlMeleeJumpData> inherited;
	
	u32				m_time_next_melee_jump;

public:
	virtual void	reinit					() override;

	virtual void	on_event				(ControlCom::EEventType, ControlCom::IEventData*) override;
	virtual void	activate				() override;
	virtual void	on_release				() override;
	virtual bool	check_start_conditions	() override;
};

