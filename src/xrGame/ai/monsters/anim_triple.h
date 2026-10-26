#pragma once

#include "control_combase.h"
#include "../../../Include/xrRender/KinematicsAnimated.h"

enum EStateAnimTriple {
	eStatePrepare,
	eStateExecute,
	eStateFinalize,
	eStateNone
};

#define TA_SKIP_PREPARE			true
#define TA_DONT_SKIP_PREPARE	false
#define TA_EXECUTE_ONCE			true
#define TA_EXECUTE_LOOPED		false


struct STripleAnimEventData : public ControlCom::IEventData {
	u32		m_current_state;
	IC		STripleAnimEventData(u32 state) : m_current_state(state) {}
};

struct SAnimationTripleData : public ControlCom::IComData {
	MotionID	pool[3];
	bool		skip_prepare;
	bool		execute_once;
	u32			capture_type;
};

class CAnimationTriple : public CControl_ComCustom<SAnimationTripleData>{
	EStateAnimTriple		m_current_state;
	EStateAnimTriple		m_previous_state;
public:
	virtual void	reset_data				() override;
	virtual void	on_capture				() override;
	virtual void	on_release				() override;
	virtual void	on_event				(ControlCom::EEventType, ControlCom::IEventData*) override;
	virtual bool	check_start_conditions	() override;
	virtual void	activate				() override;

			void	pointbreak				();
private:
			void	select_next_state		();
			void	play_selected			();
};

