#include "stdafx.h"
#pragma hdrstop
#ifdef DEBUG
#include "ode_include.h"
#include "../xrEngine/StatGraph.h"
#include "PHDebug.h"
#endif
#include "alife_space.h"
#include "hit.h"
#include "phdestroyable.h"
#include "car.h"
#include "actor.h"
#include "cameralook.h"
#include "camerafirsteye.h"
#include "script_entity_action.h"
#include "../xrEngine/xr_level_controller.h"
#include "../../Include/xrRender/Kinematics.h"
#include "level.h"
#include "CarWeapon.h"

// The generic binding lookup only searches the default action group, so
// weapon actions from agUIRadialWeapon (for example wpn_func) never reach the
// car. Resolve the car-relevant actions explicitly first.
static int CarInputAction(int cmd)
{
	if (is_binded(kDROP, cmd))
		return kDROP;
	if (is_binded(kDETECTOR, cmd))
		return kDETECTOR;
	if (is_binded(kWPN_FUNC, cmd))
		return kWPN_FUNC;
	if (is_binded(kWPN_NEXT, cmd))
		return kWPN_NEXT;
	if (is_binded(kWPN_FIREMODE_NEXT, cmd))
		return kWPN_FIREMODE_NEXT;

	return get_binded_action(cmd);
}

void	CCar::OnMouseMove(int dx, int dy)
{
	if (!IsMyCar() && !g_dedicated_server)					return;
	if(active_camera->tag == EctSecondCam || active_camera->tag == EctThridCam)
		m_camera_last_mmove_time = Device.dwTimeGlobal;
	auto CWPB = m_car_weapons.begin();
	for(;CWPB!=m_car_weapons.end();++CWPB)
	{
		if((*CWPB)->IsActive())
		{
			if(active_camera->tag == EctFirstCam)
			{
				if((*CWPB)->CameraEnabled())
				{
					(*CWPB)->OnMouseMove(dx, dy);
					return;
				}
			}
			else
				m_pPhysicsShell->Enable();
		}
	}
    Fvector cur_pos, right, left;
    cur_pos.set(0, 0, 0);
	Fvector dest_pos;
	dest_pos = Fvector(bind_center);
	XFORM().transform_tiny(dest_pos);
	float free_camera_speed = dest_pos.distance_to(active_camera->vPosition)/10;
	clamp(free_camera_speed, 3.f, 15.f);
	CCameraBase* C	= active_camera;
	float scale		= (C->f_fov/g_fov)*psMouseSens * psMouseSensScale/50.f;
	if (dx){
		float d		= float(dx)*scale;
		if (active_camera->tag == EctFiveCam)
			cur_pos.mad(active_camera->Right(), -d * free_camera_speed);
		else
			C->Move		((d<0)?kLEFT:kRIGHT, _abs(d));
	}
	if (dy){
		float d		= ((psMouseInvert)?-1:1)*float(dy)*scale*3.f/4.f;
		if (active_camera->tag == EctFiveCam)
			cur_pos.mad(active_camera->Up(), d * free_camera_speed);
		else
			C->Move		((d>0)?kUP:kDOWN, _abs(d));
	}
	if(active_camera->tag == EctFiveCam)
	{
		Fvector mov_pos = cur_pos.mul(free_camera_speed);
		active_camera->vPosition.add(mov_pos);
	}
}

void CCar::ZoomCamera(int cmd, float free_step)
{
	if (active_camera->tag == EctFirstCam)
	{
		return;
	}

	if (active_camera->tag == EctFiveCam)
	{
		const float direction = cmd == kCAM_ZOOM_IN ? 1.f : -1.f;
		active_camera->vPosition.mad(active_camera->vDirection, direction * free_step);
		return;
	}

	active_camera->Move(cmd);
}

void CCar::OnMouseWheel(int direction)
{
	if (!IsMyCar() && !g_dedicated_server)
	{
		return;
	}

	// Match the actor look-camera wheel: positive direction zooms in.
	const int cmd = direction > 0 ? kCAM_ZOOM_IN : kCAM_ZOOM_OUT;
	for (int i = 0; i < 10; ++i)
	{
		ZoomCamera(cmd, 0.1f);
	}
}
#include"../xrEngine/xr_input.h"
void CCar::OnKeyboardPress(int cmd)
{
	if (!IsMyCar() && !g_dedicated_server)								return;
	switch (CarInputAction(cmd))	
	{
		case kCAM_1:	OnCameraChange(EctFirstCam);	break;
		case kCAM_2:	OnCameraChange(EctSecondCam);	break;
		case kCAM_3:
		{
			if (active_camera->tag == EctThridCam)
				OnCameraChange(EctFourthCam);
			else if (active_camera->tag == EctFourthCam)
				OnCameraChange(EctFiveCam);
			else
				OnCameraChange(EctThridCam);
		}break;
		case kWPN_NEXT:
		{
			if (GetWeaponActive() && GetWeaponActive()->GetCurrGun())
				GetWeaponActive()->GetCurrGun()->SwitchAmmoType();
		}break;
		case kWPN_FIREMODE_PREV:
		{
			if (GetWeaponActive() && GetWeaponActive()->GetCurrGun())
				GetWeaponActive()->GetCurrGun()->OnPrevFireMode();
		}break;
		case kWPN_FIREMODE_NEXT:
		{
			if (GetWeaponActive() && GetWeaponActive()->GetCurrGun())
				GetWeaponActive()->GetCurrGun()->OnNextFireMode();
		}break;
		case kWPN_1:
			if (m_car_weapons.size()>=1) 
			{
				auto CWPB = m_car_weapons.begin();
				for(;CWPB!=m_car_weapons.end();++CWPB)
					if((*CWPB)->IsActive() && (*CWPB) != m_car_weapons[0])
						(*CWPB)->Action(CCarTurret::eWpnActivate, 0);

				m_car_weapons[0]->Action(CCarTurret::eWpnActivate, !m_car_weapons[0]->IsActive());
				if(!m_car_weapons[0]->CombinedInput())
					ReleaseInput();
			}break;
		case kWPN_2:
			if (m_car_weapons.size()>=2) 
			{
				auto CWPB = m_car_weapons.begin();
				for(;CWPB!=m_car_weapons.end();++CWPB)
					if((*CWPB)->IsActive() && (*CWPB) != m_car_weapons[1])
						(*CWPB)->Action(CCarTurret::eWpnActivate, 0);
				m_car_weapons[1]->Action(CCarTurret::eWpnActivate, !m_car_weapons[1]->IsActive());
				if(!m_car_weapons[1]->CombinedInput())
					ReleaseInput();
			}break;
		case kWPN_3:
			if (m_car_weapons.size()>=3) 
			{
				auto CWPB = m_car_weapons.begin();
				for(;CWPB!=m_car_weapons.end();++CWPB)
					if((*CWPB)->IsActive() && (*CWPB) != m_car_weapons[2])
						(*CWPB)->Action(CCarTurret::eWpnActivate, 0);
				m_car_weapons[2]->Action(CCarTurret::eWpnActivate, !m_car_weapons[2]->IsActive());
				if(!m_car_weapons[2]->CombinedInput())
					ReleaseInput();
			}break;
		case kWPN_4:
			if (m_car_weapons.size()>=4) 
			{
				auto CWPB = m_car_weapons.begin();
				for(;CWPB!=m_car_weapons.end();++CWPB)
					if((*CWPB)->IsActive() && (*CWPB) != m_car_weapons[3])
						(*CWPB)->Action(CCarTurret::eWpnActivate, 0);
				m_car_weapons[3]->Action(CCarTurret::eWpnActivate, !m_car_weapons[3]->IsActive());
				if(!m_car_weapons[3]->CombinedInput())
					ReleaseInput();
			}break;
		case kWPN_5:
			if (m_car_weapons.size()>=5) 
			{
				auto CWPB = m_car_weapons.begin();
				for(;CWPB!=m_car_weapons.end();++CWPB)
					if((*CWPB)->IsActive() && (*CWPB) != m_car_weapons[4])
						(*CWPB)->Action(CCarTurret::eWpnActivate, 0);
				m_car_weapons[4]->Action(CCarTurret::eWpnActivate, !m_car_weapons[4]->IsActive());
				if(!m_car_weapons[4]->CombinedInput())
					ReleaseInput();
			}break;
		case kWPN_6:
			if (m_car_weapons.size()>=6) 
			{
				auto CWPB = m_car_weapons.begin();
				for(;CWPB!=m_car_weapons.end();++CWPB)
					if((*CWPB)->IsActive() && (*CWPB) != m_car_weapons[5])
						(*CWPB)->Action(CCarTurret::eWpnActivate, 0);
				m_car_weapons[5]->Action(CCarTurret::eWpnActivate, !m_car_weapons[5]->IsActive());
				if(!m_car_weapons[5]->CombinedInput())
					ReleaseInput();
			}break;
		case kWPN_FIRE:
			{
				auto CWPB = m_car_weapons.begin();
				for(;CWPB!=m_car_weapons.end();++CWPB)
					if((*CWPB)->IsActive())
						(*CWPB)->Action(CCarTurret::eWpnFire, 1);
			}break;
		case kWPN_FUNC:
			{
				auto CWPB = m_car_weapons.begin();
				for(;CWPB!=m_car_weapons.end();++CWPB)
					if((*CWPB)->IsActive())
						(*CWPB)->SetGunCircle();
			}break;
	};
	auto CWPB = m_car_weapons.begin();
	for(;CWPB!=m_car_weapons.end();++CWPB)
		if((*CWPB)->IsActive() && !(*CWPB)->CombinedInput())
			return;


	switch (CarInputAction(cmd))	
	{
		// Cars-Ext uses the sprint/crouch keys for gear shifting, so changing
		// accel/crouch in the controls menu must move the shift keys as well.
		case kACCEL:	TransmissionUp();			break;
		case kCROUCH:	TransmissionDown();			break;
		case kFWD:		PressForward();				break;
		case kBACK:		PressBack();				break;
		case kR_STRAFE:	PressRight();				if (OwnerActor()) OwnerActor()->steer_Vehicle(1);break;
		case kL_STRAFE:	PressLeft();				if (OwnerActor()) OwnerActor()->steer_Vehicle(-1);break;
		case kJUMP:		PressBreaks();				break;
		case kDROP:			m_car_sound->Horn(true);	break;
		case kDETECTOR:	SwitchEngine(); SwitchEngineDoors();	break;
		case kSPRINT_TOGGLE: b_nitro_engage = true; break;
		case kTORCH:
		{
			m_lights.SwitchLights();

			if(OwnerActor() && m_torch_usage)
				OwnerActor()->UseTorch();
		}break;
		case kNIGHT_VISION: Actor()->SwitchNightVision();break;
		case kUSE:		            				 break;
	};

	switch (get_binded_action(cmd, agTransport))
	{
		case kENGINE:			SwitchEngine(); SwitchEngineDoors();	break;
		case kBRAKE:			PressBreaks();						break;
		// Explicit transmission bindings are only an alias. If the same key is
		// already used by accel/crouch/sprint_toggle (or any other action),
		// that action owns the key; otherwise sprint_toggle would shift gears.
		case kTRANSMISSION_UP:
			if (CarInputAction(cmd) == kTRANSMISSION_UP)
				TransmissionUp();
			break;
		case kTRANSMISSION_DOWN:
			if (CarInputAction(cmd) == kTRANSMISSION_DOWN)
				TransmissionDown();
			break;
	};

	if (OnClient() && Owner())
	{
		NET_Packet P;
		CGameObject::u_EventGen(P, GE_GAME_EVENT, Owner()->ID());
		P.w_u16(GAME_EVENT_MP_CAR_INPUT);
		P << ID();
		P.w_u8(cmd);
		P.w_u8(true);
		CGameObject::u_EventSend(P);
	}
}

void	CCar::OnKeyboardRelease(int cmd)
{
	if (!IsMyCar() && !g_dedicated_server)								return;
	switch (CarInputAction(cmd))	
	{
		case kACCEL:	b_auto_transmission_stop = false; break;
		case kCROUCH:	b_auto_transmission_stop = false; break;
		case kSPRINT_TOGGLE: b_nitro_engage = false; break;
		case kFWD:		ReleaseForward();			break;
		case kBACK:		ReleaseBack();				break;
		case kL_STRAFE:	ReleaseLeft();				if (OwnerActor()) OwnerActor()->steer_Vehicle(0);	break;
		case kR_STRAFE:	ReleaseRight();				if (OwnerActor()) OwnerActor()->steer_Vehicle(0);	break;
		case kDROP: m_car_sound->Horn(false); break;
		case kJUMP:		ReleaseBreaks();			break;
		case kWPN_FIRE:
		{
			auto CWPB = m_car_weapons.begin();
			for(;CWPB!=m_car_weapons.end();++CWPB)
				if((*CWPB)->IsActive())
					(*CWPB)->Action(CCarTurret::eWpnFire, 0);
		}break;
	};

	switch (get_binded_action(cmd, agTransport))
	{
		case kBRAKE:	ReleaseBreaks();	break;
	};

	if (OnClient() && Owner())
	{
		NET_Packet P;
		CGameObject::u_EventGen(P, GE_GAME_EVENT, Owner()->ID());
		P.w_u16(GAME_EVENT_MP_CAR_INPUT);
		P << ID();
		P.w_u8(cmd);
		P.w_u8(false);
		CGameObject::u_EventSend(P);
	}
}
void	CCar::OnKeyboardHold(int cmd)
{
	if (!IsMyCar() && !g_dedicated_server)								return;
	auto CWPB = m_car_weapons.begin();
	for(;CWPB!=m_car_weapons.end();++CWPB)
		if((*CWPB)->IsActive() && !(*CWPB)->CombinedInput())
			return;
	m_steer_boost = 0.f;
	float boost_left = 1.f;
	float boost_right = 1.f;
	float boost_speed = 1.f - GetSpeedKmH() * 0.002f;
	clamp(boost_speed, 0.5f, 1.f);

	if (GetSpeedKmH() > 10.f)
	{
		if (m_steer_speed_boost && !b_burnout)
		{
			if (GetVelocity().dotproduct(XFORM().k) > 0.f || fwp)
			{
				if (GetAVelocity().dotproduct(XFORM().j) > 0.f)
				{
					boost_left = 1.f + m_steer_counter_turn_factor * GetAVelocity().dotproduct(XFORM().j); //counter twist
					boost_right = 1.f - m_steer_turn_factor * GetAVelocity().dotproduct(XFORM().j);
				}
				else
				{
					boost_left = 1.f + m_steer_turn_factor * GetAVelocity().dotproduct(XFORM().j);
					boost_right = 1.f - m_steer_counter_turn_factor * GetAVelocity().dotproduct(XFORM().j); //counter twist
				}
			}


			if (boost_left < 0.f)
			{
				boost_left = boost_left * m_steer_turn_compensation + m_steer_turn_delta;
				clamp(boost_left, boost_left, 0.f);
			}
			if (boost_right < 0.f)
			{
				boost_right = boost_right * m_steer_turn_compensation + m_steer_turn_delta;
				clamp(boost_right, boost_right, 0.f);
			}

			float speed = (GetSpeedKmH() - 20.f) * 0.005f;
			clamp(speed, 0.f, m_steer_turn_compensation_max);
			clamp(boost_left, -speed, boost_left);
			clamp(boost_right, -speed, boost_right);

			if (GetVelocity().dotproduct(XFORM().k) < 0.f && !fwp)
			{
				boost_left *= -0.5f;
				boost_right *= -0.5f;
			}
		}
	}
	else
	{
		if (m_steer_spin_boost && b_burnout)
		{
			boost_left -= GetSpeedKmH() * 0.1f;
			boost_right -= GetSpeedKmH() * 0.1f;
		}
	}


	float boost_brake = GetSpeedKmH() * 0.01f;
	float brake = 0.f;

    Fvector cur_pos, right, left;
    cur_pos.set(0, 0, 0);
	float free_camera_speed = 7.f;
	switch (CarInputAction(cmd))
	{
		case kACCEL:	b_auto_transmission_stop = true; break;
		case kCROUCH:	b_auto_transmission_stop = true; break;
		case kSPRINT_TOGGLE: b_nitro_engage = true; break;
		case kBACK: if (OwnerActor() && b_engine_on) brake = boost_brake * (m_brake_boost * Device.fTimeDelta);	break;
		case kR_STRAFE:	if (OwnerActor() && b_engine_on && (m_steer_speed_boost || m_steer_spin_boost)) m_steer_boost = boost_speed * boost_right * Device.fTimeDelta;	break;
		case kL_STRAFE:	if (OwnerActor() && b_engine_on && (m_steer_speed_boost || m_steer_spin_boost)) m_steer_boost = boost_speed * boost_left * -(Device.fTimeDelta); break;
		case kDROP: m_car_sound->Horn(true); break;
		case kCAM_ZOOM_IN:
			ZoomCamera(kCAM_ZOOM_IN, 2.f * Device.fTimeDelta);
			break;
		case kCAM_ZOOM_OUT:
			ZoomCamera(kCAM_ZOOM_OUT, 2.f * Device.fTimeDelta);
			break;
		case kUP:
		{
			if(active_camera->tag == EctFiveCam)
			{
				cur_pos.mad(active_camera->vDirection, free_camera_speed / 2.0f);
				break;
			}
		}
		case kDOWN:
		{
			if(active_camera->tag == EctFiveCam)
			{
				cur_pos.mad(active_camera->vDirection, -free_camera_speed / 2.0f);
				break;
			}
		}
		case kLEFT:
		{
			if(active_camera->tag == EctFiveCam)
			{
				left.set(Fvector(active_camera->Right()));
				cur_pos.mad(left, -free_camera_speed / 2.0f);
				break;
			}
		}
		case kRIGHT:
		{
			if(active_camera->tag == EctFiveCam)
			{
				right.set(active_camera->Right());
				cur_pos.mad(right, free_camera_speed / 2.0f);
				break;
			}
			else
				active_camera->Move(cmd);
		}break;
	}

	if(active_camera->tag == EctFiveCam)
	{
		Fvector mov_pos = cur_pos.mul((free_camera_speed) * Device.fTimeDelta);
		active_camera->vPosition.add(mov_pos);
	}

	if (m_pPhysicsShell && m_pPhysicsShell->isActive())
	{
		m_pPhysicsShell->Enable();

		if (m_pPhysicsShell->get_ElementByStoreOrder(0) && mb_traction)
		{
			if (GetSpeedKmH() > 10.f)
			{
				if (m_steer_speed_boost && !b_burnout)
				{
					Fvector angular_velocity = GetAvelAxes(0);
					angular_velocity.y += m_steer_boost * m_steer_speed_boost;
					SetAvelAxes(0, angular_velocity);
				}
			}
			else
			{
				if (m_steer_spin_boost && b_burnout)
				{
					Fvector angular_velocity = GetAvelAxes(0);
					angular_velocity.y += m_steer_boost * m_steer_spin_boost;
					SetAvelAxes(0, angular_velocity);
				}
			}

			if (m_brake_boost && m_pPhysicsShell->get_ElementByStoreOrder(0) && mb_traction && b_breaks && !b_burnout && m_current_slip_sliding < m_slip_sliding_threshold)
			{
				Fvector linear_velocity = GetLvelAxes(0);
				linear_velocity.z -= brake;
				SetLvelAxes(0, linear_velocity);
			}
		}
	}
}

void CCar::ReleaseInput()
{
	if (!IsMyCar() && !g_dedicated_server) return;
	b_auto_transmission_stop = false;
	b_auto_transmission_stop = false;
	b_nitro_engage = false;
	ReleaseForward();
	ReleaseBack();
	ReleaseLeft();				
	ReleaseRight();
	m_car_sound->Horn(false);
	ReleaseBreaks();
	if (OwnerActor()) OwnerActor()->steer_Vehicle(0);
}

bool CCar::bfAssignMovement(CScriptEntityAction *tpEntityAction)
{
	if (tpEntityAction->m_tMovementAction.m_bCompleted)
		return(false);

	u32		l_tInput = tpEntityAction->m_tMovementAction.m_tInputKeys;

	vfProcessInputKey(kFWD		,	!!(l_tInput & CScriptMovementAction::eInputKeyForward		));
	vfProcessInputKey(kBACK		,	!!(l_tInput & CScriptMovementAction::eInputKeyBack		));
	vfProcessInputKey(kL_STRAFE	,	!!(l_tInput & CScriptMovementAction::eInputKeyLeft		));
	vfProcessInputKey(kR_STRAFE	,	!!(l_tInput & CScriptMovementAction::eInputKeyRight		));
	vfProcessInputKey(kACCEL	,	!!(l_tInput & CScriptMovementAction::eInputKeyShiftUp		));
	vfProcessInputKey(kCROUCH	,	!!(l_tInput & CScriptMovementAction::eInputKeyShiftDown	));
	vfProcessInputKey(kJUMP		,	!!(l_tInput & CScriptMovementAction::eInputKeyBreaks		));
	if (!!(l_tInput & CScriptMovementAction::eInputKeyEngineOn))	StartEngine();
	if (!!(l_tInput & CScriptMovementAction::eInputKeyEngineOff)) StopEngine();

	//if (_abs(tpEntityAction->m_tMovementAction.m_fSpeed) > EPS_L)
		//m_current_rpm = _abs(tpEntityAction->m_tMovementAction.m_fSpeed*m_current_gear_ratio);

	return	(true);
}

bool CCar::bfAssignObject(CScriptEntityAction *tpEntityAction)
{
	CScriptObjectAction	&l_tObjectAction = tpEntityAction->m_tObjectAction;
	if (l_tObjectAction.m_bCompleted || !xr_strlen(l_tObjectAction.m_caBoneName))
		return((l_tObjectAction.m_bCompleted = true) == false);

	s16	l_sBoneID = smart_cast<IKinematics*>(Visual())->LL_BoneID(l_tObjectAction.m_caBoneName);
	if (is_Door(l_sBoneID)) {
		switch(l_tObjectAction.m_tGoalType) {
			case MonsterSpace::eObjectActionActivate : {
				if (!DoorOpen(l_sBoneID))
					return((l_tObjectAction.m_bCompleted = true) == false);
				break;
			}
			case MonsterSpace::eObjectActionDeactivate : {
				if (!DoorClose(l_sBoneID))
					return((l_tObjectAction.m_bCompleted = true) == false);
				break;
			}
			case MonsterSpace::eObjectActionUse : {
				if (!DoorSwitch(l_sBoneID))
					return((l_tObjectAction.m_bCompleted = true) == false);
				break;
			}
			default : 
				return	((l_tObjectAction.m_bCompleted = true) == false);
		}
		return		(false);
	}
	SCarLight* light=NULL;
	if (m_lights.findLight(l_sBoneID,light)) {
		switch(l_tObjectAction.m_tGoalType) {
			case MonsterSpace::eObjectActionActivate : {
				light->TurnOn();
				return		((l_tObjectAction.m_bCompleted = true) == false);
			}
			case MonsterSpace::eObjectActionDeactivate : {
				light->TurnOff();
				return		((l_tObjectAction.m_bCompleted = true) == false);
			}
			case MonsterSpace::eObjectActionUse : {
				light->Switch();
				return		((l_tObjectAction.m_bCompleted = true) == false);
			}
			default : 
				return	((l_tObjectAction.m_bCompleted = true) == false);
		}
	
	}
	
	return			(false);
}

void CCar::vfProcessInputKey	(int iCommand, bool bPressed)
{
	if (bPressed)
		OnKeyboardPress			(iCommand);
	else
		OnKeyboardRelease		(iCommand);
}

void CCar::Action(u16 id, u32 flags)
{
	if(m_car_weapons.size())m_car_weapons[0]->Action(id,flags);
}
void CCar::SetParam(int id, Fvector2 val)
{
	if(m_car_weapons.size())m_car_weapons[0]->SetParam(id,val);
}
void CCar::SetParam			(int id, Fvector val)
{
	if(m_car_weapons.size())m_car_weapons[0]->SetParam(id,val);
}
bool CCar::WpnCanHit()
{
	if(m_car_weapons.size()) return m_car_weapons[0]->AllowFire();
	return false;
}

float CCar::FireDirDiff()
{
	if(m_car_weapons.size()) return m_car_weapons[0]->FireDirDiff();
	return 0.0f;
}
#include "script_game_object.h"
#include "car_memory.h"
#include "visual_memory_manager.h"

bool CCar::isObjectVisible			(CScriptGameObject* O_)
{
	if(m_memory)
	{
		return m_memory->visual().visible_now(&O_->object());
	}else
	{

		if(!O_)
		{
			Msg("Attempt to call CCar::isObjectVisible method wihth passed NULL parameter");
			return false;
		}
		CObject* O = &O_->object();
		Fvector dir_to_object;
		Fvector to_point;
		O->Center(to_point);
		
		Fvector				from_point;
		Center				(from_point);

		if(HasWeapon())
			from_point.y		= XFORM().c.y + m_car_weapons[0]->_height();

		dir_to_object.sub(to_point,from_point).normalize_safe();
		float ray_length = from_point.distance_to(to_point);


		BOOL res = Level().ObjectSpace.RayTest(from_point, dir_to_object, ray_length, collide::rqtStatic, NULL, NULL);
		return (0==res);
	}
}

bool CCar::HasWeapon()
{
	return !!m_car_weapons.size();
}

bool CCar::HasWeaponActive()
{
	bool wp_active = false;
	auto CWPB = m_car_weapons.begin();
	for(;CWPB!=m_car_weapons.end();++CWPB)
		if((*CWPB)->IsActive() && (*CWPB)->GetGunsSize())
			wp_active = true;

	return wp_active;
}

CCarTurret *CCar::GetWeaponActive()
{
	auto CWPB = m_car_weapons.begin();
	for(;CWPB!=m_car_weapons.end();++CWPB)
		if((*CWPB)->IsActive() && (*CWPB)->GetGunsSize())
			return (*CWPB);

	return NULL;
}

float CCar::HasWeaponActiveDisp()
{
	float disp = 0.0f;
	auto CWPB = m_car_weapons.begin();
	for(;CWPB!=m_car_weapons.end();++CWPB)
		if((*CWPB)->IsActive())
			disp = (*CWPB)->GetDispersion();

	return disp;
}

Fvector CCar::CurrentVel()
{
	Fvector lin_vel;
	m_pPhysicsShell->get_LinearVel(lin_vel);

	return lin_vel;
}
void CCar::OnGamepadAxisMove(int id, Fvector2 value)
{
	if (!IsMyCar())
		return;

	if (id == 0)
	{
		if (!fis_zero(value.x))
			(value.x > 0.f) ? PressRight() : PressLeft();
		else if (pInput->GetControllerMode())
		{
			ReleaseLeft();
			ReleaseRight();
		}

		if (OwnerActor())
			OwnerActor()->steer_Vehicle(value.x);
	}
	else if (id == 1)
	{
		CCameraBase* C = active_camera;
		float scale = (C->f_fov / g_fov) * psGamepadSens * Device.fTimeDelta * psMouseSensScale;
		if (value.x)
		{
			float d = value.x * scale * 8;
			C->Move((d < 0) ? kLEFT : kRIGHT, std::abs(d));
		}

		if (value.y)
		{
			float d = (psGamepadInvert ? -1 : 1) * value.y * scale * 3.f / 4.f;
			d *= 8;
			C->Move((d > 0) ? kUP : kDOWN, std::abs(d));
		}
	}
	else if (id == 2)
	{
		if (!fis_zero(value.x))
			PressBack();
		else if (pInput->GetControllerMode())
			ReleaseBack();

		if (!fis_zero(value.y))
			PressForward();
		else if (pInput->GetControllerMode())
			ReleaseForward();
	}
}

void CCar::OnGyroscopeMove(Fvector3 value)
{
	if (!IsMyCar())
		return;

	CCameraBase* C = active_camera;
	float scale = (C->f_fov / g_fov) * Device.fTimeDelta * psMouseSensScale;
	if (value.x)
	{
		float d = (psGyroscopeInvertX ? -1 : 1) * value.x * scale;
		C->Move((d < 0) ? kUP : kDOWN, std::abs(d));
	}

	if (value.y)
	{
		float d = (psGyroscopeInvertY ? -1 : 1) * value.y * scale;
		C->Move((d > 0) ? kLEFT : kRIGHT, std::abs(d));
	}
}

void CCar::OnGamepadKeyPress(int id)
{
	if (!IsMyCar() && !g_dedicated_server)
		return;

	switch (get_binded_action(id))
	{
	case kACCEL:	TransmissionUp();	break;
	case kCROUCH:	TransmissionDown();	break;
	case kCAM_2:
		if (active_camera->tag != EctSecondCam)
			OnCameraChange(EctSecondCam);
		else
			OnCameraChange(EctFirstCam);
		break;
	case kTORCH:	m_lights.SwitchLights();	break;
	case kUSE:		break;
	};

	switch (get_binded_action(id, agTransport))
	{
	case kENGINE:			SwitchEngine();		break;
	case kBRAKE:			PressBreaks();		break;
	case kTRANSMISSION_UP:
		if (get_binded_action(id) == kTRANSMISSION_UP)
			TransmissionUp();
		break;
	case kTRANSMISSION_DOWN:
		if (get_binded_action(id) == kTRANSMISSION_DOWN)
			TransmissionDown();
		break;
	};

	if (OnClient() && Owner())
	{
		NET_Packet P;
		CGameObject::u_EventGen(P, GE_GAME_EVENT, Owner()->ID());
		P.w_u16(GAME_EVENT_MP_CAR_INPUT);
		P << ID();
		P.w_u8(get_binded_action(id));
		P.w_u8(true);
		CGameObject::u_EventSend(P);
	}
}

void CCar::OnGamepadKeyRelease(int id)
{
	if (!IsMyCar() && !g_dedicated_server)
		return;

	switch (get_binded_action(id, agTransport))
	{
	case kBRAKE:	ReleaseBreaks();	break;
	};

	if (OnClient() && Owner())
	{
		NET_Packet P;
		CGameObject::u_EventGen(P, GE_GAME_EVENT, Owner()->ID());
		P.w_u16(GAME_EVENT_MP_CAR_INPUT);
		P << ID();
		P.w_u8(id);
		P.w_u8(false);
		CGameObject::u_EventSend(P);
	}
}
