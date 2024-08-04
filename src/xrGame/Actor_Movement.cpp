#include "stdafx.h"

#include "Actor.h"
#include "Inventory.h"
#include "Weapon.h"
#include "../xrEngine/CameraBase.h"
#include "xrMessages.h"

#include "Level.h"
#include "UIGameCustom.h"
#include "actorcondition.h"
#include "game_cl_base.h"
#include "WeaponMagazined.h"
#include "CharacterPhysicsSupport.h"
#include "actoreffector.h"
#include "player_hud.h"

#ifdef DEBUG
#include "phdebug.h"
#endif
static const float	s_fLandingTime1		= 0.1f;// через сколько снять флаг Landing1 (т.е. включить следующую анимацию)
static const float	s_fLandingTime2		= 0.3f;// через сколько снять флаг Landing2 (т.е. включить следующую анимацию)
static const float	s_fJumpTime			= 0.3f;
static const float	s_fJumpGroundTime	= 0.1f;	// для снятия флажка Jump если на земле
	   const float	s_fFallTime			= 0.2f;
	   float f_CoefReturnSpeed = 11.5f;
	   float s_fDecreaseSpeed = 14.f;
	   float m_fDecreaseWalkAccel;

IC static void generate_orthonormal_basis1(const Fvector& dir,Fvector& updir, Fvector& right)
{

	right.crossproduct(dir,updir); //. <->
	right.normalize();
	updir.crossproduct(right,dir);
}


void CActor::g_cl_ValidateMState(float dt, u32 mstate_wf)
{
	// Lookout
	if (mstate_wf&mcLLookout && mstate_wf&mcRLookout)
		mstate_real &= ~mcLookout;
	else if (mstate_wf & mcLookout)
		mstate_real	|= mstate_wf&mcLookout;
	else
		mstate_real	&= ~mcLookout;
		
	if (mstate_real&(mcJump|mcFall|mcLanding|mcLanding2))
		mstate_real		&= ~mcLookout;

	// закончить приземление
	if (mstate_real&(mcLanding|mcLanding2)){
		m_fLandingTime		-= dt;
		if (m_fLandingTime<=0.f){
			mstate_real		&=~	(mcLanding|mcLanding2);
			mstate_real		&=~	(mcFall|mcJump);
		}
	}
	// закончить падение
	if (character_physics_support()->movement()->gcontact_Was){
		if (mstate_real&mcFall){
			if (character_physics_support()->movement()->GetContactSpeed()>4.f){
				if (fis_zero(character_physics_support()->movement()->gcontact_HealthLost)){	
					m_fLandingTime	= s_fLandingTime1;
					mstate_real		|= mcLanding;
				}else{
					m_fLandingTime	= s_fLandingTime2;
					mstate_real		|= mcLanding2;
				}
			}
		}
		m_bJumpKeyPressed	=	TRUE;
		m_fJumpTime			=	s_fJumpTime;
		mstate_real			&=~	(mcFall|mcJump);
	}
	if ((mstate_wf&mcJump)==0)	
		m_bJumpKeyPressed	=	FALSE;

	// Зажало-ли меня/уперся - не двигаюсь
	if (((character_physics_support()->movement()->GetVelocityActual()<0.2f)&&(!(mstate_real&(mcFall|mcJump)))) || character_physics_support()->movement()->bSleep) 
	{
		mstate_real				&=~ mcAnyMove;
	}
	if (character_physics_support()->movement()->Environment()==CPHMovementControl::peOnGround || character_physics_support()->movement()->Environment()==CPHMovementControl::peAtWall)
	{
		// если на земле гарантированно снимать флажок Jump
		if (((s_fJumpTime-m_fJumpTime)>s_fJumpGroundTime)&&(mstate_real&mcJump))
		{
			mstate_real			&=~	mcJump;
			m_fJumpTime			= s_fJumpTime;
		}
	}
	if(character_physics_support()->movement()->Environment()==CPHMovementControl::peAtWall)
	{
		if(!(mstate_real & mcClimb))
		{
			mstate_real				|=mcClimb;
			mstate_real				&=~mcSprint;
			cam_SetLadder();
		}
	}
	else
	{
		if (mstate_real & mcClimb)
		{
			cam_UnsetLadder();
		}
		mstate_real				&=~mcClimb;		
	};

	if (mstate_wf != mstate_real){
		if ((mstate_real&mcCrouch)&&((0==(mstate_wf&mcCrouch)) || mstate_real&mcClimb)){
			if (character_physics_support()->movement()->ActivateBoxDynamic(0)){
				mstate_real &= ~mcCrouch;
			}
		}
	}

	if(!CanAccelerate()&&isActorAccelerated(mstate_real, IsZoomAimingMode()))
	{
		mstate_real				^=mcAccel;
	};	

	if (this == Level().CurrentControlEntity())
	{
		bool bOnClimbNow			= !!(mstate_real&mcClimb);
		bool bOnClimbOld			= !!(mstate_old&mcClimb);

		if (bOnClimbNow != bOnClimbOld )
		{
			SetWeaponHideState		(INV_STATE_LADDER, bOnClimbNow );
		};
	};

	if (mstate_real & mcSprint) {
		if (eacLookAt == cam_active && (mstate_real & mcJump)) {
			mstate_wishful ^= mcAccel; // interrupt sprinting when jumping to avoid model shaking
		}
	}
	else {
		if (eacLookAt == cam_active && (mstate_old & mcSprint) && (mstate_real & mcAccel) &&
			m_bJumpKeyPressed) {
			mstate_wishful ^= mcSprint; // continue sprinting
		}
	}
};

void CActor::g_cl_CheckControls(u32 mstate_wf, Fvector &vControlAccel, float &Jump, float dt)
{
	float					cam_eff_factor = 0.0f;
	mstate_old				= mstate_real;
	vControlAccel.set		(0,0,0);

	if (!(mstate_real&mcFall) && (character_physics_support()->movement()->Environment()==CPHMovementControl::peInAir)) 
	{
		m_fFallTime				-=	dt;
		if (m_fFallTime<=0.f)
		{
			m_fFallTime			=	s_fFallTime;
			mstate_real			|=	mcFall;
			mstate_real			&=~	mcJump;
		}
	}

	if(!CanMove()) 
	{
		if(mstate_wf&mcAnyMove) 
		{
			StopAnyMove();
			mstate_wf &= ~mcAnyMove;
			mstate_wf &= ~mcJump;
		}
	}
	// update player accel
	if (mstate_wf&mcFwd)		vControlAccel.z +=  1;
	if (mstate_wf&mcBack)		vControlAccel.z += -1;
	if (mstate_wf&mcLStrafe)	vControlAccel.x += -1;
	if (mstate_wf&mcRStrafe)	vControlAccel.x +=  1;

	CPHMovementControl::EEnvironment curr_env = character_physics_support()->movement()->Environment();
	if(curr_env==CPHMovementControl::peOnGround || curr_env==CPHMovementControl::peAtWall)
	{
		// crouch
		if ((0==(mstate_real&mcCrouch))&&(mstate_wf&mcCrouch))
		{
			if(mstate_real&mcClimb)
			{
				mstate_wf&=~mcCrouch;
			}
			else
			{
				character_physics_support()->movement()->EnableCharacter();
				bool Crouched = false;
				if(isActorAccelerated(mstate_wf, IsZoomAimingMode()))
					Crouched = character_physics_support()->movement()->ActivateBoxDynamic(1);
				else
					Crouched = character_physics_support()->movement()->ActivateBoxDynamic(2);
				
				if(Crouched) 
					mstate_real			|=	mcCrouch;
			}
		}
		// jump
		m_fJumpTime				-=	dt;

		if (m_fDecreaseWalkAccel > 0)
			m_fDecreaseWalkAccel -= dt * f_CoefReturnSpeed;
		if (m_fDecreaseWalkAccel < 0)
			m_fDecreaseWalkAccel = 0;

		if( CanJump() && (mstate_wf&mcJump) )
		{
			mstate_real			|=	mcJump;
			m_bJumpKeyPressed	=	TRUE;
			Jump				= m_fJumpSpeed;
			m_fJumpTime			= s_fJumpTime;
			m_fDecreaseWalkAccel = s_fDecreaseSpeed;

			//уменьшить силу игрока из-за выполненого прыжка
			if (!GodMode())
				conditions().ConditionJump(inventory().TotalWeight() / MaxCarryWeight());
		}

		// mask input into "real" state
		u32 move	= mcAnyMove|mcAccel;

		if(mstate_real&mcCrouch)
		{
			if (!isActorAccelerated(mstate_real, IsZoomAimingMode()) && isActorAccelerated(mstate_wf, IsZoomAimingMode()))
			{
				character_physics_support()->movement()->EnableCharacter();
				if(!character_physics_support()->movement()->ActivateBoxDynamic(1))move	&=~mcAccel;
			}

			if (isActorAccelerated(mstate_real, IsZoomAimingMode()) && !isActorAccelerated(mstate_wf, IsZoomAimingMode()))
			{
				character_physics_support()->movement()->EnableCharacter();
				if(character_physics_support()->movement()->ActivateBoxDynamic(2))mstate_real	&=~mcAccel;
			}
		}

		if ((mstate_wf&mcSprint) && !CanSprint())
			mstate_wf				&= ~mcSprint;

		mstate_real &= (~move);
		mstate_real |= (mstate_wf & move);

		if(mstate_wf&mcSprint)
			mstate_real|=mcSprint;
		else
			mstate_real&=~mcSprint;
		if (!(mstate_real & (mcFwd | mcLStrafe | mcRStrafe)) ||
			(mstate_real & mcFwd && mstate_real & mcBack) ||
			(mstate_real & mcLStrafe && mstate_real & mcRStrafe) ||
			mstate_real & (mcCrouch | mcClimb) ||
			!isActorAccelerated(mstate_wf, IsZoomAimingMode())) {
			mstate_real &= ~mcSprint;
			mstate_wishful &= ~mcSprint;
		}
				
		// check player move state
		if(mstate_real&mcAnyMove)
		{
			BOOL	bAccelerated		= isActorAccelerated(mstate_real, IsZoomAimingMode())&&CanAccelerate();

			// correct "mstate_real" if opposite keys pressed
			if (_abs(vControlAccel.z)<EPS)	mstate_real &= ~(mcFwd + mcBack + mcSprint);
			if (_abs(vControlAccel.x)<EPS)	mstate_real &= ~(mcLStrafe+mcRStrafe);

			// normalize and analyze crouch and run
			float	scale			= vControlAccel.magnitude();
			if(scale>EPS)	
			{	
				if (eacLookAt != cam_active)
					scale = (m_fWalkAccel - m_fDecreaseWalkAccel) / scale;
				else
					scale = m_fWalkAccel / scale;
				if (bAccelerated)
					if (mstate_real&mcBack)
						scale *= m_fRunBackFactor;
					else
						scale *= m_fRunFactor;
				else
					if (mstate_real&mcBack)
						scale *= m_fWalkBackFactor;



				if (mstate_real&mcCrouch)	scale *= m_fCrouchFactor;
				if (mstate_real&mcClimb)	scale *= m_fClimbFactor;
				if (mstate_real&mcSprint)	scale *= m_fSprintFactor;

				if (mstate_real&(mcLStrafe|mcRStrafe) && !(mstate_real&mcCrouch))
				{
					if (bAccelerated)
						scale *= m_fRun_StrafeFactor;
					else
						scale *= m_fWalk_StrafeFactor;
				}

				vControlAccel.mul			(scale);
				cam_eff_factor				= scale;
			}//scale>EPS
		}//(mstate_real&mcAnyMove)
	}//peOnGround || peAtWall

	bool isGuns = EngineExternal()[EEngineExternalGunslinger::EnableGunslingerMode];
	if (IsGameTypeSingle() && (!isGuns && cam_eff_factor > EPS || isGuns))
	{
		ECamEffectorType anm_id = eCEActorMoving;
		xr_string state_anm = "";
		xr_string HUD_PREFIX = "cam_";
		CWeapon* wpn = smart_cast<CWeapon*>(inventory().ActiveItem());
		if (mstate_real & mcSprint)
		{
			state_anm = "sprint";
			if (wpn)
				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());

			anm_id = eCEActorMovingSprint;
		}
		else if (mstate_real & mcLStrafe && !(mstate_old & mcLStrafe))
		{
			state_anm = "strafe_left";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorMovingLeft;
		}
		else if (mstate_real & mcRStrafe && !(mstate_old & mcRStrafe))
		{
			state_anm = "strafe_right";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorMovingRight;
		}
		else if (mstate_real & mcFwd && !(mstate_old & mcFwd))
		{
			state_anm = "move_fwd";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorMovingFwd;
		}
		else if (mstate_real & mcBack && !(mstate_old & mcBack))
		{
			state_anm = "move_back";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorMovingBack;
		}
		else if (mstate_real & mcLanding)
		{
			state_anm = "landing";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorMovingLanding;
		}
		else if (mstate_real & mcLanding2)
		{
			state_anm = "landing2";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorMovingLanding;
		}
		else if (mstate_real & mcJump && !(mstate_old & mcJump))
		{
			state_anm = "jump";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorMovingJump;
		}
		else if (mstate_real & mcFall && !(mstate_old & mcFall))
		{
			state_anm = "fall";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorMovingFall;
		}
		else if (!(mstate_real & mcLLookout) && mstate_wishful & mcLLookout)
		{
			state_anm = "lookout_left_start";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorLLookoutStart;
		}
		else if (!(mstate_real & mcRLookout) && mstate_wishful & mcRLookout)
		{
			state_anm = "lookout_right_start";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorRLookoutStart;
		}
		else if (mstate_real & mcLLookout && !(mstate_wishful & mcLLookout))
		{
			state_anm = "lookout_left_end";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorLLookoutEnd;
		}
		else if (mstate_real & mcRLookout && !(mstate_wishful & mcRLookout))
		{
			state_anm = "lookout_right_end";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorRLookoutEnd;
		}
		else if (mstate_real & mcCrouch && !(mstate_old & mcCrouch))
		{
			state_anm = "crouch_down";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorMovingCrouchDown;
		}
		else if (mstate_real & mcCrouch && !(mstate_wishful & mcCrouch))
		{
			state_anm = "crouch_up";
			if (wpn)
			{
				if (wpn->IsZoomed())
					state_anm += "_aim";

				state_anm = READ_IF_EXISTS(pSettings, r_string, wpn->HudSection(), (HUD_PREFIX + state_anm).c_str(), state_anm.c_str());
			}

			anm_id = eCEActorMovingCrouchUp;
		}

		if (state_anm != "")
		{ //play moving cam effect
			CActor*	control_entity = static_cast<CActor*>(Level().CurrentControlEntity());
			R_ASSERT2(control_entity, "current control entity is nullptr");
			CEffectorCam* ec = control_entity->Cameras().GetCamEffector(anm_id);
			if (!ec)
			{
				string_path	eff_name;
				xr_sprintf(eff_name, sizeof(eff_name), "%s.anm", state_anm.c_str());
				string_path	ce_path;
				string_path	anm_name;
				xr_strconcat(anm_name, "camera_effects\\actor_move\\", eff_name);
				if (FS.exist(ce_path, "$game_anims$", anm_name))
				{
					CAnimatorCamLerpEffectorConst* e = new CAnimatorCamLerpEffectorConst();
					float factor = isGuns ? 70.0f : cam_eff_factor / 70.0f;
					e->SetFactor(factor);
					e->SetType(anm_id);
					e->SetHudAffect(false);
					e->SetCyclic(false);
					e->Start(anm_name);
					control_entity->Cameras().AddCamEffector(e);
				}
			}
		}
	}
	//transform local dir to world dir
	Fmatrix				mOrient;
	mOrient.rotateY		(-r_model_yaw);
	mOrient.transform_dir(vControlAccel);
}

#define ACTOR_ANIM_SECT "actor_animation"

#define ACTOR_LLOOKOUT_ANGLE	PI_DIV_4
#define ACTOR_RLOOKOUT_ANGLE	PI_DIV_4

void CActor::g_Orientate	(u32 mstate_rl, float dt)
{
	static float fwd_l_strafe_yaw	= deg2rad(pSettings->r_float(ACTOR_ANIM_SECT,	"fwd_l_strafe_yaw"));
	static float back_l_strafe_yaw	= deg2rad(pSettings->r_float(ACTOR_ANIM_SECT,	"back_l_strafe_yaw"));
	static float fwd_r_strafe_yaw	= deg2rad(pSettings->r_float(ACTOR_ANIM_SECT,	"fwd_r_strafe_yaw"));
	static float back_r_strafe_yaw	= deg2rad(pSettings->r_float(ACTOR_ANIM_SECT,	"back_r_strafe_yaw"));
	static float l_strafe_yaw		= deg2rad(pSettings->r_float(ACTOR_ANIM_SECT,	"l_strafe_yaw"));
	static float r_strafe_yaw		= deg2rad(pSettings->r_float(ACTOR_ANIM_SECT,	"r_strafe_yaw"));

	if(!g_Alive())return;
	// visual effect of "fwd+strafe" like motion
	float calc_yaw = 0;
	if(mstate_real&mcClimb)
	{
		if(g_LadderOrient()) return;
	}
	switch(mstate_rl&mcAnyMove)
	{
	case mcFwd+mcLStrafe:
		calc_yaw = +fwd_l_strafe_yaw;//+PI_DIV_4; 
		break;
	case mcBack+mcRStrafe:
		calc_yaw = +back_r_strafe_yaw;//+PI_DIV_4; 
		break;
	case mcFwd+mcRStrafe:
		calc_yaw = -fwd_r_strafe_yaw;//-PI_DIV_4; 
		break;
	case mcBack+mcLStrafe: 
		calc_yaw = -back_l_strafe_yaw;//-PI_DIV_4; 
		break;
	case mcLStrafe:
		calc_yaw = +l_strafe_yaw;//+PI_DIV_3-EPS_L; 
		break;
	case mcRStrafe:
		calc_yaw = -r_strafe_yaw;//-PI_DIV_4+EPS_L; 
		break;
	}

	float scale_yaw_offset = 1.0f;

	if(cam_active == eacFirstEye && g_player_hud && g_player_hud->m_legs_model) {
	 	scale_yaw_offset = 0.15f;
	}

	// lerp angle for "effect" and capture torso data from camera
	angle_lerp(r_model_yaw_delta, calc_yaw * scale_yaw_offset, PI_MUL_4 * scale_yaw_offset, dt);

	// build matrix
	Fmatrix mXFORM;
	mXFORM.rotateY	(-(r_model_yaw + r_model_yaw_delta));
	mXFORM.c.set	(Position());
	XFORM().set		(mXFORM);
	VERIFY(_valid(XFORM()));

	//-------------------------------------------------

	float tgt_roll		=	0.f;
	if (mstate_rl&mcLookout)
	{
		tgt_roll		=	(mstate_rl&mcLLookout)?-ACTOR_LLOOKOUT_ANGLE:ACTOR_RLOOKOUT_ANGLE;
		
		if( (mstate_rl&mcLLookout) && (mstate_rl&mcRLookout) )
			tgt_roll	= 0.0f;
	}
	if (!fsimilar(tgt_roll,r_torso_tgt_roll,EPS)){
		r_torso_tgt_roll = angle_inertion_var(r_torso_tgt_roll, tgt_roll, 0.f, CurrentHeight * PI_MUL_2, PI_DIV_2, dt);
		r_torso_tgt_roll= angle_normalize_signed(r_torso_tgt_roll);
	}
}
bool CActor::g_LadderOrient()
{
	Fvector leader_norm;
	character_physics_support()->movement()->GroundNormal(leader_norm);
	if(_abs(leader_norm.y)>M_SQRT1_2) return false;
	//leader_norm.y=0.f;
	float mag=leader_norm.magnitude();
	if(mag<EPS_L) return false;
	leader_norm.div(mag);
	leader_norm.invert();
	Fmatrix M;M.set(Fidentity);
	M.k.set(leader_norm);
	M.j.set(0.f,1.f,0.f);
	generate_orthonormal_basis1(M.k,M.j,M.i);
	M.i.invert();
	//M.j.invert();


	//Fquaternion q1,q2,q3;
	//q1.set(XFORM());
	//q2.set(M);
	//q3.slerp(q1,q2,dt);
	//Fvector angles1,angles2,angles3;
	//XFORM().getHPB(angles1.x,angles1.y,angles1.z);
	//M.getHPB(angles2.x,angles2.y,angles2.z);
	////angle_lerp(angles3.x,angles1.x,angles2.x,dt);
	////angle_lerp(angles3.y,angles1.y,angles2.y,dt);
	////angle_lerp(angles3.z,angles1.z,angles2.z,dt);

	//angles3.lerp(angles1,angles2,dt);
	////angle_lerp(angles3.y,angles1.y,angles2.y,dt);
	////angle_lerp(angles3.z,angles1.z,angles2.z,dt);
	//angle_lerp(angles3.x,angles1.x,angles2.x,dt);
	//XFORM().setHPB(angles3.x,angles3.y,angles3.z);
	Fvector position;
	position.set(Position());
	//XFORM().rotation(q3);
	VERIFY2(_valid(M),"Invalide matrix in g_LadderOrient");
	XFORM().set(M);
	VERIFY2(_valid(position),"Invalide position in g_LadderOrient");
	Position().set(position);
	VERIFY(_valid(XFORM()));
	return true;
}
// ****************************** Update actor orientation according to camera orientation
void CActor::g_cl_Orientate	(u32 mstate_rl, float dt)
{
	// capture camera into torso (only for FirstEye & LookAt cameras)
	if (eacFreeLook!=cam_active)
	{
		r_torso.yaw		=	cam_Active()->GetWorldYaw	();
		r_torso.pitch	=	cam_Active()->GetWorldPitch	();
	}
	else
	{
		r_torso.yaw		=	cam_FirstEye()->GetWorldYaw	();
		r_torso.pitch	=	cam_FirstEye()->GetWorldPitch	();
	}

	unaffected_r_torso.yaw		= r_torso.yaw;
	unaffected_r_torso.pitch	= r_torso.pitch;
	unaffected_r_torso.roll		= r_torso.roll;

	CWeaponMagazined *pWM = smart_cast<CWeaponMagazined*>(inventory().GetActiveSlot() != NO_ACTIVE_SLOT ? 
		inventory().ItemFromSlot(inventory().GetActiveSlot()) : nullptr);
	if (pWM && pWM->GetCurrentFireMode() == 1 && eacFirstEye != cam_active)
	{
		Fvector dangle = weapon_recoil_last_delta();
		r_torso.yaw		=	unaffected_r_torso.yaw + dangle.y;
		r_torso.pitch	=	unaffected_r_torso.pitch + dangle.x;
	}
	
	if (eacLookAt == cam_active && inventory().GetActiveSlot() == NO_ACTIVE_SLOT)
		r_torso.pitch = 0;

	// если есть движение - выровнять модель по камере
	if(mstate_rl & mcAnyMove || (g_player_hud && g_player_hud->m_legs_model)) {
		r_model_yaw		= angle_normalize(r_torso.yaw);
		mstate_real		&=~mcTurn;
	} else {
		// if camera rotated more than 45 degrees - align model with it
		float ty = angle_normalize(r_torso.yaw);
		if (_abs(r_model_yaw-ty)>PI_DIV_4)	{
			r_model_yaw_dest = ty;
			// 
			mstate_real	|= mcTurn;
		}
		if (_abs(r_model_yaw-r_model_yaw_dest)<EPS_L){
			mstate_real	&=~mcTurn;
		}
		if (mstate_rl&mcTurn){
			angle_lerp	(r_model_yaw,r_model_yaw_dest,PI_MUL_2,dt);
		}
	}
}

void CActor::g_sv_Orientate(u32 /**mstate_rl/**/, float /**dt/**/)
{
	r_model_yaw		= NET_Last.o_model;

	r_torso.yaw		=	unaffected_r_torso.yaw;
	r_torso.pitch	=	unaffected_r_torso.pitch;
	r_torso.roll	=	unaffected_r_torso.roll;

	CWeaponMagazined *pWM = smart_cast<CWeaponMagazined*>(inventory().GetActiveSlot() != NO_ACTIVE_SLOT ? 
		inventory().ItemFromSlot(inventory().GetActiveSlot()) : nullptr);
	if (pWM && pWM->GetCurrentFireMode() == 1/* && eacFirstEye != cam_active*/)
	{
		Fvector dangle = weapon_recoil_last_delta();
		r_torso.yaw		+=	dangle.y;
		r_torso.pitch	+=	dangle.x;
		r_torso.roll	+=	dangle.z;
	}
}

bool isActorAccelerated(u32 mstate, bool ZoomMode) 
{
	bool res = false;
	if (mstate&mcAccel)
		res = false;
	else
		res = true;

	if (mstate&(mcCrouch|mcClimb|mcJump|mcLanding|mcLanding2))
		return res;
	if (mstate & mcLookout || ZoomMode)
		return false;
	return res;
}

bool CActor::CanAccelerate()
{
	bool can_accel = !conditions().IsLimping() &&
		!character_physics_support()->movement()->PHCapture() && 
		(m_time_lock_accel < Device.dwTimeGlobal)
	;		

	return can_accel;
}

bool CActor::CanRun()
{
	bool can_run		= !IsZoomAimingMode() && !(mstate_real&mcLookout);
	return can_run;
}

bool CActor::CanSprint()
{
	bool can_Sprint = CanAccelerate() && !conditions().IsCantSprint() && Game().PlayerCanSprint(this) && CanRun() && !(mstate_real & mcLStrafe || mstate_real & mcRStrafe) && InventoryAllowSprint() && !bBlockSprint;

	return can_Sprint && (m_block_sprint_counter<=0);
}

bool CActor::CanJump()
{
	bool can_Jump = 
		!character_physics_support()->movement()->PHCapture() &&((mstate_real&mcJump)==0) && (m_fJumpTime<=0.f) 
		&& !m_bJumpKeyPressed &&!IsZoomAimingMode();

	return can_Jump;
}

bool CActor::CanMove()
{
	if( conditions().IsCantWalk() )
	{
		if(mstate_wishful&mcAnyMove)
		{
			CurrentGameUI()->AddCustomStatic("cant_walk", true);
		}
		return false;
	}else
	if( conditions().IsCantWalkWeight() )
	{
		if(mstate_wishful&mcAnyMove)
		{
			CurrentGameUI()->AddCustomStatic("cant_walk_weight", true);
		}
		return false;
	
	}

	if(IsTalking())
		return false;
	else
		return true;
}

void CActor::StopAnyMove()
{
	mstate_wishful	&=		~mcAnyMove;
	mstate_real		&=		~mcAnyMove;

	if (this == Level().CurrentViewEntity())
	{
		g_player_hud->OnMovementChanged((EMoveCommand)0);
	}
}


bool CActor::is_jump()
{
	return ((mstate_real & (mcJump|mcFall|mcLanding|mcLanding2)) != 0);
}

//максимальный переносимы вес
#include "CustomOutfit.h"
float CActor::MaxCarryWeight () const
{
	float res = inventory().GetMaxWeight();
	res      += get_additional_weight();
	return res;
}

float CActor::MaxWalkWeight() const
{
	float max_w = CActor::conditions().MaxWalkWeight();
	max_w      += get_additional_weight();
	return max_w;
}
#include "Artefact.h"
float CActor::get_additional_weight() const
{
	float res = 0.0f ;
	CCustomOutfit* outfit	= GetOutfit();
	if ( outfit )
	{
		res				+= outfit->m_additional_weight;
	}

	for(TIItemContainer::const_iterator it = inventory().m_belt.begin(); 
		inventory().m_belt.end() != it; ++it) 
	{
		CArtefact*	artefact = smart_cast<CArtefact*>(*it);
		if(artefact)
			res			+= artefact->AdditionalInventoryWeight();
	}

	return res;
}

void CActor::SetMovementState(const ACTOR_DEFS::EMovementStates& state, const ACTOR_DEFS::EMoveCommand& mask, bool status)
{
	switch (state)
	{
        case ACTOR_DEFS::EMovementStates::eReal:
		{
			if (status)
				mstate_real |= mask;
			else
				mstate_real &= ~mask;
		}break;
        case ACTOR_DEFS::EMovementStates::eWishful:
		{
			if (status)
				mstate_wishful |= mask;
			else
				mstate_wishful &= ~mask;
		}break;
        case ACTOR_DEFS::EMovementStates::eOld:
		{
			if (status)
				mstate_old |= mask;
			else
				mstate_old &= ~mask;
		}break;
	}
}

u32 CActor::GetMovementState(const ACTOR_DEFS::EMovementStates& state) const
{
	u32 result = 0;
	switch (state)
	{
		case ACTOR_DEFS::EMovementStates::eReal:
			result = mstate_real;
		break;
        case ACTOR_DEFS::EMovementStates::eWishful:
			result = mstate_wishful;
		break;
        case ACTOR_DEFS::EMovementStates::eOld:
			result = mstate_old;
		break;
	}

	return result;
}