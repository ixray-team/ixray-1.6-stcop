#pragma once
#include "IInputReceiver.h"

class CCameraBase;

class IGame_Actor:
	public IInputReceiver
{
public:
	virtual xr_vector<xr_string> GetKnownPortions() const = 0;
	virtual xr_vector<xr_string> GetKnownPortionDialogs(shared_str id) const = 0;
	virtual xr_vector<xr_string> GetKnownPortionDisable(shared_str id) const = 0;
	virtual xr_vector<xr_string> GetKnownPortionArticles(shared_str id) const = 0;
	virtual xr_vector<xr_string> GetKnownPortionArticlesDisable(shared_str id) const = 0;
	virtual xr_vector<xr_string> GetKnownPortionTasks(shared_str id) const = 0;

	virtual void GiveInfoPortion(const char* infoPortion) = 0;
	virtual void DisableInfoPortion(const char* info_id) = 0;
	virtual void SetActorPosition(Fvector pos) = 0;
	virtual void SetActorDirection(float dir) = 0;
	virtual void StopAnyMove() = 0;
	virtual void RenderItemUI() = 0;
	virtual void UpdatePlayerHud() = 0;

	virtual CCameraBase* cam_Active() { return nullptr; }
};

extern ENGINE_API IGame_Actor* g_pIGameActor;

extern ENGINE_API int psActorSleepTime;
