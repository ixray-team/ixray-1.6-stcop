#pragma once

#include "control_combase.h"
#include "../../../Include/xrRender/KinematicsAnimated.h"

struct SAnimationSequencerData : public ControlCom::IComData {
	xr_vector<MotionID>	motions;
};
	
class CAnimationSequencer : public CControl_ComCustom<SAnimationSequencerData> {
	u32						m_index;
public:
	virtual void	reset_data				() override;
	virtual void	on_capture				() override;
	virtual void	on_release				() override;
	virtual void	on_event				(ControlCom::EEventType, ControlCom::IEventData*) override;

	virtual bool	check_start_conditions	() override;

	virtual void	activate				() override;
private:
			void	play_selected			();
};

