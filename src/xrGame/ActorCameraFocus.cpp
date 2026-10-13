#include "StdAfx.h"
#include "Actor.h"
#include "ActorEffector.h"
#include "Inventory.h"
#include "Level.h"
#include "Weapon.h"
#include "../xrEngine/CameraBase.h"

bool CActor::CanFocusCamera() const
{
	return IsGameTypeSingle() && !g_dedicated_server && g_Alive() && !getDestroy() && Level().CurrentEntity() == this && Level().CurrentControlEntity() == this && cam_active == eacFirstEye && !m_holder && !IsTalking() && HasCameraEffector() && !m_pActorEffector->AbsolutePositioning() && !Level().Cameras().GetCamEffector(cefDemo);
}

bool CActor::FocusCameraPoint(const Fvector& Point, const CameraFocusParams& Params)
{
	if (!CanFocusCamera() || !_valid(Point) || !Params.IsValid())
	{
		return false;
	}
	CCameraBase* Camera = cam_FirstEye();
	if (!Focus.Start(Params, Camera->yaw, Camera->pitch, Cameras().Fov()))
	{
		return false;
	}
	FocusTarget = nullptr;
	FocusPoint = Point;
	FocusRestoreDirection = Params.RestoreDirection;
	Camera->StopLookingAtPoint();
	lastTimeAutoAimStarted = 0;
	if (Params.LockInput)
	{
		StopAnyMove();
		mstate_wishful &= ~(mcJump | mcSprint | mcLookout);
		m_iKeyFlags = 0;
		inventory().Action(kWPN_FIRE, CMD_STOP);
		if (CWeapon* Weapon = inventory().ActiveItem() ? inventory().ActiveItem()->cast_weapon() : nullptr)
		{
			Weapon->OnZoomOut();
		}
	}
	return true;
}

bool CActor::FocusCameraObject(CGameObject* Target, const Fvector& Offset, const CameraFocusParams& Params)
{
	if (!Target || Target == this || Target->getDestroy() || !_valid(Offset))
	{
		return false;
	}
	Fvector Point;
	Point.add(Target->Position(), Offset);
	if (!FocusCameraPoint(Point, Params))
	{
		return false;
	}
	FocusOffset = Offset;
	FocusTarget = Params.Track ? Target : nullptr;
	return true;
}

void CActor::StopCameraFocus(float Duration)
{
	const bool WasActive = Focus.IsActive();
	Focus.Stop(Duration);
	if (Focus.GetPhase() == CameraFocus::Phase::Leaving || !Focus.IsActive())
	{
		FocusTarget = nullptr;
	}
	if (WasActive && !Focus.IsActive() && FocusRestoreDirection)
	{
		cam_FirstEye()->yaw = Focus.GetYaw();
		cam_FirstEye()->pitch = Focus.GetPitch();
	}
}

void CActor::ResetCameraFocus()
{
	Focus.Reset();
	FocusTarget = nullptr;
}

void CActor::ReleaseCameraFocusTarget(CObject* Object)
{
	if (FocusTarget == Object)
	{
		FocusTarget = nullptr;
		StopCameraFocus();
	}
}

float CActor::UpdateCameraFocus(const Fvector& Position, float NaturalFov, float Delta)
{
	if (!Focus.IsActive())
	{
		return NaturalFov;
	}
	if (!CanFocusCamera())
	{
		ResetCameraFocus();
		return NaturalFov;
	}
	if (FocusTarget)
	{
		if (FocusTarget->getDestroy())
		{
			StopCameraFocus();
		}
		else
		{
			FocusPoint.add(FocusTarget->Position(), FocusOffset);
		}
	}
	CCameraBase* Camera = cam_FirstEye();
	Fvector Direction;
	Direction.sub(FocusPoint, Position);
	float TargetYaw = Focus.GetYaw();
	float TargetPitch = Focus.GetPitch();
	if (_valid(Direction) && Direction.square_magnitude() > EPS_S)
	{
		Fmatrix Rotation;
		Rotation.identity();
		Rotation.k.normalize(Direction);
		Fvector::generate_orthonormal_basis(Rotation.k, Rotation.j, Rotation.i);
		Fvector Angles;
		Rotation.getXYZi(Angles);
		TargetYaw = Angles.y;
		TargetPitch = Angles.x;
		if (Camera->bClampPitch)
		{
			clamp(TargetPitch, Camera->lim_pitch.x, Camera->lim_pitch.y);
		}
		if (Camera->bClampYaw)
		{
			clamp(TargetYaw, Camera->lim_yaw.x, Camera->lim_yaw.y);
		}
	}
	const bool ControlsDirection = Focus.GetPhase() != CameraFocus::Phase::Leaving || FocusRestoreDirection;
	Focus.Update(Delta, TargetYaw, TargetPitch, NaturalFov);
	if (ControlsDirection)
	{
		Camera->StopLookingAtPoint();
		Camera->yaw = Focus.GetYaw();
		Camera->pitch = Focus.GetPitch();
	}
	if (!Focus.IsActive() || Focus.GetPhase() == CameraFocus::Phase::Leaving)
	{
		FocusTarget = nullptr;
	}
	return Focus.IsActive() ? Focus.GetFov() : NaturalFov;
}
