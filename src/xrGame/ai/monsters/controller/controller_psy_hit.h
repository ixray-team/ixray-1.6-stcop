#pragma once
#include "../control_combase.h"
#include "../../../../Include/xrRender/KinematicsAnimated.h"

class CPsyHitEffectorCam;
class CPsyHitEffectorPP;	
class CActor;

class CControllerPsyHit : public CControl_ComCustom<> {
protected:
	using inherited = CControl_ComCustom<>;

	MotionID			m_stage[4];
	u8					m_current_index;

	CPsyHitEffectorCam	*m_effector_cam;
	CPsyHitEffectorPP	*m_effector_pp;
	u16						m_curent_actor_id = u16(-1);


	enum ESoundState{
		ePrepare,
		eStart,
		ePull,
		eHit,
		eNone
	} m_sound_state;


	float				m_min_tube_dist;

	// internal flag if weapon was hidden
	bool				m_blocked;

	u32					m_time_last_tube;

public:
	CControllerPsyHit();
	virtual ~CControllerPsyHit() override;

	virtual void	load					(LPCSTR section) override;
	virtual	void	reinit					() override;
	virtual	void	update_frame			() override;
	virtual bool	check_start_conditions	() override;
	virtual void	activate				() override;
	virtual void	deactivate				() override;
	
	virtual void	on_event				(ControlCom::EEventType, ControlCom::IEventData*) override;

			void	on_death				();
			bool	tube_ready				() const;

private:

			void	stop					();

			void	play_anim				();
			void	death_glide_start		();
			void	death_glide_end			();

			void	set_sound_state			(ESoundState state);
			void	hit						();
			bool	check_conditions_final	();

			bool	see_enemy(CActor* pA);
};

