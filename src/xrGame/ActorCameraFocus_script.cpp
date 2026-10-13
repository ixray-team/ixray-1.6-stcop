#include "StdAfx.h"
#include "script_game_object.h"

bool CScriptGameObject::ActorFocusPoint(const Fvector& Point, const CameraFocusParams& Params)
{
	CActor* Actor = object().cast_actor();
	return Actor && Actor->FocusCameraPoint(Point, Params);
}

bool CScriptGameObject::ActorFocusObject(CScriptGameObject* Target, const Fvector& Offset, const CameraFocusParams& Params)
{
	CActor* Actor = object().cast_actor();
	return Actor && Target && Actor->FocusCameraObject(&Target->object(), Offset, Params);
}

void CScriptGameObject::ActorStopFocus(float Duration)
{
	if (CActor* Actor = object().cast_actor())
	{
		Actor->StopCameraFocus(Duration);
	}
}

bool CScriptGameObject::ActorFocusActive() const
{
	CActor* Actor = object().cast_actor();
	return Actor && Actor->IsCameraFocusActive();
}

u32 CScriptGameObject::ActorFocusState() const
{
	CActor* Actor = object().cast_actor();
	return Actor ? Actor->GetCameraFocusState() : 0;
}
