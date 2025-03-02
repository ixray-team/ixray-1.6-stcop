#pragma once
#include "../control_combase.h"
#include "../../../../Include/xrRender/KinematicsAnimated.h"

class CPsyHitEffectorCam;
class CPsyHitEffectorPP;	
class CActor;
class CController;

class CControllerPsyHit : public CControl_ComCustom<> {
	typedef CControl_ComCustom<> inherited;

	MotionID			m_stage[4]{};
	u8					m_current_index{};

	CPsyHitEffectorCam* m_effector_cam{};
	CPsyHitEffectorPP	*m_effector_pp;
	u16						m_curent_actor_id = u16(-1);


	enum ESoundState{
		ePrepare,
		eStart,
		ePull,
		eHit,
		eNone
	} m_sound_state;

	struct controller_feel_params
	{
		float min_dist = 0.0f;
		float max_dist = 0.0f;
	};


	float				m_min_tube_dist{};
	float				m_controller_prepare_time{};
	float				m_controller_psyblocked_time{};
	float				m_controller_time{};
	float				m_controller_queue_stop_prob{};
	// internal flag if weapon was hidden
	bool				m_blocked{};

	u32					m_time_last_tube{};
	controller_feel_params m_feel_params{};

public:
	virtual void	load					(LPCSTR section);
	virtual	void	reinit					();
	virtual	void	update_frame			();
	virtual bool	check_start_conditions	();
	virtual void	activate				();
	virtual void	deactivate				();
	
	virtual void	on_event				(ControlCom::EEventType, ControlCom::IEventData*);

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
			bool	PsiStart				(CController* monster_controller);
			bool	PsiEffects				(CController* monster_controller);
			void	OnPsyHitActivate		(CController* monster_controller);
			bool	IsNeedPsiHitOverride	();

			bool	see_enemy(CActor* pA);

};

