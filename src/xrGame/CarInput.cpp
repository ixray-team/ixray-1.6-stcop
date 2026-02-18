#include "StdAfx.h"

#ifdef DEBUG
#	include "PHDebug.h"
#endif
#include "alife_space.h"
#include "Hit.h"
#include "PHDestroyable.h"
#include "Car.h"
#include "Actor.h"
#include "cameralook.h"
#include "CameraFirstEye.h"
#include "script_entity_action.h"
#include "../xrEngine/xr_level_controller.h"
#include "../Include/xrRender/Kinematics.h"
#include "Level.h"
#include "CarWeapon.h"
#include "../xrEngine/xr_input.h"
#include "script_game_object.h"
#include "visual_memory_manager.h"

void CCar::OnMouseMove(int dx, int dy)
{
	if (!IsMyCar())
		return;

	CCameraBase* C = active_camera;
	float Scale = (C->f_fov / g_fov) * psMouseSens * psMouseSensScale / 50.f;
	if (dx)
	{
		float Delta = float(dx) * Scale;
		C->Move((Delta < 0) ? kLEFT : kRIGHT, std::abs(Delta));
	}

	if (dy)
	{
		float Delta = (psMouseInvert ? -1 : 1) * float(dy) * Scale * 3.f / 4.f;
		C->Move((Delta > 0) ? kUP : kDOWN, std::abs(Delta));
	}
}

void CCar::OnGamepadAxisMove(int id, Fvector2 value)
{
	if (!IsMyCar())
		return;

	// left stick
	if (id == 0)
	{
		if (!fis_zero(value.x))
		{
			(value.x > 0.f) ? PressRight() : PressLeft();
		}
		else if (pInput->GetControllerMode())
		{
			ReleaseLeft();
			ReleaseRight();
		}

		if (OwnerActor())
		{
			OwnerActor()->steer_Vehicle(value.x);
		}
	}
	// right stick
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
	// triggers
	else if (id == 2)
	{
		if (!fis_zero(value.x))
		{
			PressBack();
		}
		else if (pInput->GetControllerMode())
		{
			ReleaseBack();
		}

		if (!fis_zero(value.y))
		{
			PressForward();
		}
		else if (pInput->GetControllerMode())
		{
			ReleaseForward();
		}
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

	s16	l_sBoneID = PKinematics(Visual())->LL_BoneID(l_tObjectAction.m_caBoneName);
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
	SCarLight* light=nullptr;
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
		OnKeyboardPress			(get_action_dik((EGameActions)iCommand));
	else
		OnKeyboardRelease		(get_action_dik((EGameActions)iCommand));
}

void CCar::OnKeyboardPress(int dik)
{
	if (!IsMyCar() && !g_dedicated_server)
		return;

	switch (get_binded_action(dik))	
	{
	case kCAM_1:	OnCameraChange(ectFirst);	break;
	case kCAM_2:
		if (active_camera->tag != ectChase)
			OnCameraChange(ectChase);
		else
			OnCameraChange(ectFirst);
		break;
	case kCAM_3:	OnCameraChange(ectFree);	break;
	case kFWD:		PressForward();				break;
	case kBACK:		PressBack();				break;
	case kR_STRAFE:	PressRight();				if (OwnerActor()) OwnerActor()->steer_Vehicle(1);	break;
	case kL_STRAFE:	PressLeft();				if (OwnerActor()) OwnerActor()->steer_Vehicle(-1);break;
	case kDETECTOR: 
		SwitchEngine();
		break;
	case kTORCH:	m_lights.SwitchHeadLights();break;
	case kUSE:									break;
	};

	switch (get_binded_action(dik, agTransport))
	{
	case kTRANSMISSION_UP:		
		TransmissionUp();		
		break;
	case kTRANSMISSION_DOWN:	
		TransmissionDown();			
		break;
	case kBRAKE:		
		PressBreaks();				
		break;
	case kENGINE:
		SwitchEngine();
		break;
	};

	if (OnClient())
	{
		NET_Packet P;
		CGameObject::u_EventGen(P, GE_GAME_EVENT, Owner()->ID());
		P.w_u16(GAME_EVENT_MP_CAR_INPUT);
		P << ID();
		P.w_u8(dik);
		P.w_u8(true);
		CGameObject::u_EventSend(P);
	}
}

void CCar::OnGamepadKeyPress(int id)
{
	if (!IsMyCar() && !g_dedicated_server)
		return;

	switch (get_binded_action(id))	
	{
	case kCAM_2:
		if (active_camera->tag != ectChase)
			OnCameraChange(ectChase);
		else
			OnCameraChange(ectFirst);
		break;
	case kTORCH:	m_lights.SwitchHeadLights();break;
	case kUSE:									break;
	};

	switch (get_binded_action(id, agTransport))
	{
	case kENGINE:
		SwitchEngine();
		break;
	case kBRAKE:		
		PressBreaks();				
		break;
	case kTRANSMISSION_UP:	
		TransmissionUp();			
		break;
	case kTRANSMISSION_DOWN:	
		TransmissionDown();			
		break;
	};

	if (OnClient())
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

void CCar::OnKeyboardRelease(int dik)
{
	if (!IsMyCar() && !g_dedicated_server)
		return;

	switch (get_binded_action(dik))	
	{
	case kFWD:		ReleaseForward();			break;
	case kBACK:		ReleaseBack();				break;
	case kL_STRAFE:	ReleaseLeft();				if (OwnerActor()) OwnerActor()->steer_Vehicle(0);	break;
	case kR_STRAFE:	ReleaseRight();				if (OwnerActor()) OwnerActor()->steer_Vehicle(0);	break;
	};

	switch (get_binded_action(dik, agTransport))
	{
	case kBRAKE:		ReleaseBreaks();			break;
	};

	if (OnClient())
	{
		NET_Packet P;
		CGameObject::u_EventGen(P, GE_GAME_EVENT, Owner()->ID());
		P.w_u16(GAME_EVENT_MP_CAR_INPUT);
		P << ID();
		P.w_u8(dik);
		P.w_u8(false);
		CGameObject::u_EventSend(P);
	}
}

void CCar::OnGamepadKeyRelease(int id)
{
	if (!IsMyCar() && !g_dedicated_server)
		return;

	switch (get_binded_action(id, agTransport))
	{
	case kBRAKE:		ReleaseBreaks();			break;
	};

	if (OnClient())
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

void CCar::OnKeyboardHold(int cmd)
{
	if (!IsMyCar())
		return;

	switch(cmd)
	{
	case kCAM_ZOOM_IN: 
	case kCAM_ZOOM_OUT: 
	case kUP:
	case kDOWN:
	case kLEFT:
	case kRIGHT:	active_camera->Move(cmd);	break;
	}
}
void CCar::Action(u16 id, u32 flags)
{
	if(m_car_weapon)m_car_weapon->Action(id,flags);
}
void CCar::SetParam(int id, Fvector2 val)
{
	if(m_car_weapon)m_car_weapon->SetParam(id,val);
}
void CCar::SetParam			(int id, Fvector val)
{
	if(m_car_weapon)m_car_weapon->SetParam(id,val);
}
bool CCar::WpnCanHit()
{
	if(m_car_weapon) return m_car_weapon->AllowFire();
	return false;
}

float CCar::FireDirDiff()
{
	if(m_car_weapon) return m_car_weapon->FireDirDiff();
	return 0.0f;
}

bool CCar::isObjectVisible(CScriptGameObject* O_)
{
	if (!O_)
	{
		Msg("Attempt to call CCar::isObjectVisible method wihth passed nullptr parameter");
		return false;
	}

	CObject* O = &O_->object();
	Fvector Dist2Obj;
	Fvector Point;
	O->Center(Point);

	Fvector From;
	Center(From);

	if (HasWeapon())
	{
		From.y = XFORM().c.y + m_car_weapon->_height();
	}

	Dist2Obj.sub(Point, From).normalize_safe();
	float RayLen = From.distance_to(Point);

	bool res = Level().ObjectSpace.RayTest(From, Dist2Obj, RayLen, collide::rqtStatic, nullptr, nullptr);
	return (0 == res);
}

bool CCar::HasWeapon()
{
	return (m_car_weapon != nullptr);
}

Fvector CCar::CurrentVel()
{
	Fvector lin_vel;
	m_pPhysicsShell->get_LinearVel(lin_vel);

	return lin_vel;
}
