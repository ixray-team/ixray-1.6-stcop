#pragma once
#include "FFxExtendedEntityesLogic/Perceptions/CELSharedPerceptionProfiles.h"
#include "xrGame/GamePersistent.h"
#include "xrEngine/Environment.h"

class CELSharedEnvironment
{
public:
	static float Rain()
	{
		return g_pGamePersistent && g_pGamePersistent->Environment().CurrentEnv ? std::max(0.f, g_pGamePersistent->Environment().CurrentEnv->rain_density) : 0.f;
	}
	static float Fog()
	{
		return g_pGamePersistent && g_pGamePersistent->Environment().CurrentEnv ? std::max(0.f, g_pGamePersistent->Environment().CurrentEnv->fog_density) : 0.f;
	}
	static float Daylight()
	{
		if (!g_pGamePersistent)
		{
			return 1.f;
		}
		const float Hour = g_pGamePersistent->Environment().GetGameTime() / 3600.f;
		return std::min(clampr((Hour - 5.f) / 2.f, 0.f, 1.f), clampr((21.f - Hour) / 2.f, 0.f, 1.f));
	}
	static float Vision(const SELPerceptionProfile& Profile)
	{
		const float Light = Daylight();
		const float RainLoss = clampr(Rain() * Profile.RainVisionLoss, 0.f, .15f);
		const float FogLoss = clampr(Fog() * Profile.FogVisionLoss, 0.f, .4f);
		// Weather penalties compete; only the stronger one reduces the day/night range.
		return (Profile.NightVision + (Profile.DayVision - Profile.NightVision) * Light) * (1.f - std::max(RainLoss, FogLoss));
	}
	static float Acquisition(const SELPerceptionProfile& Profile)
	{
		return Profile.NightAcquire + (Profile.DayAcquire - Profile.NightAcquire) * Daylight();
	}
	static bool CanSmell() { return Rain() <= .1f; }
	static float CloseFactor() { return CanSmell() ? 1.f : .5f; }
};
