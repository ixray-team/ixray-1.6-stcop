#pragma once
#include "IInputReceiver.h"

class IGame_Actor:
	public IInputReceiver
{
public:
	virtual xr_vector<xr_string> GetKnowedPortions() const = 0;

	virtual void GiveInfoPortion(const char* infoPortion) = 0;
	virtual void DisableInfoPortion(const char* info_id) = 0;
	virtual void SetActorPosition(Fvector pos) = 0;
	virtual void SetActorDirection(float dir) = 0;
	virtual void RenderItemUI() = 0;
	virtual void StopAnyMove() = 0;
};

extern ENGINE_API IGame_Actor* g_pIGameActor;

extern ENGINE_API int psActorSleepTime;
