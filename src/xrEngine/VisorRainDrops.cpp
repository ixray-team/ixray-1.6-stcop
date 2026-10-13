#include "stdafx.h"
#include "VisorRainDrops.h"
#include "IGame_Persistent.h"
#include "IGame_Level.h"
#include "Rain.h"

void VisorRainDrops::Update(bool IsEnabled)
{
	CObject* CurrentViewEntity = g_pGameLevel ? g_pGameLevel->CurrentViewEntity() : nullptr;
	if (!IsEnabled || !CurrentViewEntity || !g_pGamePersistent)
	{
		Reset();
		return;
	}

	if (ViewEntity != CurrentViewEntity)
	{
		Reset();
		ViewEntity = CurrentViewEntity;
	}

	if (LastFrame == Device.dwFrame)
	{
		return;
	}
	LastFrame = Device.dwFrame;

	const CEnvironment& Environment = g_pGamePersistent->Environment();
	if (!Environment.CurrentEnv || !Environment.eff_Rain || !Environment.eff_Rain->IsEnabled)
	{
		Clear();
		return;
	}

	const float RainDensity = clampr(Environment.CurrentEnv->rain_density, 0.f, 1.f);
	if (RainDensity < EPS_L)
	{
		Clear();
		return;
	}

	if (!HasRayPick || Device.dwTimeGlobal - LastRayPickTime >= 1000)
	{
		collide::rq_result Result;
		IsUnderCover = g_pGameLevel->ObjectSpace.RayPick(Device.vCameraPosition, Fvector().set(0.f, 1.f, 0.f), 50.f, collide::rqtBoth, Result, CurrentViewEntity);
		LastRayPickTime = Device.dwTimeGlobal;
		HasRayPick = true;
	}

	if (!Device.Paused())
	{
		// Coverage changes by 0.05 per second (20 seconds to full coverage).
		const float Change = 0.05f * clampr(Device.fTimeDelta, 0.f, 1.f);
		Amount = clampr(Amount + (IsUnderCover ? -Change : Change), 0.f, RainDensity);
	}
}

void VisorRainDrops::Clear()
{
	Amount = 0.f;
}

void VisorRainDrops::Reset()
{
	Clear();
	LastFrame = u32(-1);
	LastRayPickTime = 0;
	HasRayPick = false;
	IsUnderCover = true;
	ViewEntity = nullptr;
}
