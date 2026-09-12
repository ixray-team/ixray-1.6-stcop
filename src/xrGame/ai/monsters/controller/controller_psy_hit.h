#pragma once
#include "../control_combase.h"
#include "../../../../Include/xrRender/KinematicsAnimated.h"

class CPsyHitEffectorCam;
class CPsyHitEffectorPP;
class CActor;
class CController;

class CControllerPsyHit : public CControl_ComCustom<>
{
	typedef CControl_ComCustom<> inherited;

	MotionID m_stage[4];
	u8 m_current_index;

	CPsyHitEffectorCam* m_effector_cam;
	CPsyHitEffectorPP* m_effector_pp;
	u16 m_curent_actor_id = u16(-1);


	enum ESoundState
	{
		ePrepare,
		eStart,
		ePull,
		eHit,
		eNone
	} m_sound_state;


	float m_min_tube_dist;

	// internal flag if weapon was hidden
	bool m_blocked;

	u32 m_time_last_tube;

public:
	virtual void load(const char* section);
	virtual void reinit();
	virtual void update_frame();
	virtual bool check_start_conditions();
	virtual void activate();
	virtual void deactivate();

	virtual void on_event(ControlCom::EEventType, ControlCom::IEventData*);

	void on_death();
	bool tube_ready() const;

	struct ControllerFeelParams
	{
		float MinDist = 0.0f;
		float MaxDist = 0.0f;
	} FeelParams = {};

	struct ÑontrollerPsiUnBlockParams
	{
		float MinDist = 0.0f;
		float MaxDist = 0.0f;

		float MinDistProb = 0.0f;
		float MaxDistProb = 0.0f;
	} PsiUnBlockParams = {};

	float ControllerPsyBlockedTime = 0.0f;
	float ControllerTime = 0.0f;
	float ControllerQueueStopProb = 0.0f;

	bool PsiStart(CController* monster_controller);
	bool PsiEffects(CController* monster_controller, CActor* Actor);
	void OnPsyHitActivate(CController* monster_controller);
	void UpdatePsiBlockFailedState(CController* monster_controller, CActor* Actor);
	bool IsNeedPsiHitOverride();
	bool TryFeelActor(CActor* Actor);
	bool see_enemy(CActor* pA);

	static bool EnableSuicide;

private:
	void stop();

	void play_anim();
	void death_glide_start();
	void death_glide_end();

	void set_sound_state(ESoundState state);
	void hit();
	bool check_conditions_final();
};
