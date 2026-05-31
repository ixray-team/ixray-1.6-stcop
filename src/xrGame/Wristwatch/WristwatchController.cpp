#include "StdAfx.h"
#include "WristwatchController.h"
#include "WristwatchFont.h"

#include "../Actor.h"
#include "../ActorCondition.h"
#include "../Level.h"
#include "../player_hud.h"
#include "../ui/UIMotionIcon.h"

#include "../../xrCore/API/xrAPI.h"
#include "../../xrEngine/device.h"
#include "../../xrEngine/date_time.h"
#include "../../xrEngine/WristwatchSettings.h"
#include "../../xrCore/Collision/ISpatial.h"

void CWristwatchController::LoadSettings()
{
	ReloadWristwatchRuntimeSettings();
	_settings = GetWristwatchRuntimeSettings().game;
	_hasAppliedDisplayType = false;
	WristwatchFont::Invalidate();
	WristwatchFont::EnsureLoaded();
}

EWristwatchSurgeMode CWristwatchController::ResolveSurgeMode(const SWristwatchSurgeState& surgeState) const
{
	if (surgeState.mode == EWristwatchSurgeMode::Normal &&
		surgeState.untilSurgeSeconds > 0 &&
		surgeState.untilSurgeSeconds <= _settings.preSurgeWindow)
	{
		return EWristwatchSurgeMode::PreSurgeFreeze;
	}

	return surgeState.mode;
}

u64 CWristwatchController::ResolveDisplayGameTime(u64 liveGameTime, EWristwatchSurgeMode surgeMode)
{
	if (surgeMode == EWristwatchSurgeMode::Normal)
	{
		_hasFrozenTime = false;
		return liveGameTime;
	}

	if (!_hasFrozenTime)
	{
		_frozenGameTime = liveGameTime;
		_hasFrozenTime = true;
	}

	return _frozenGameTime;
}

CWristwatchController::SVisibilityState CWristwatchController::ResolveVisibility(EWristwatchSurgeMode surgeMode) const
{
	SVisibilityState visibility;

	switch (_settings.displayType)
	{
	case EWristwatchDisplayType::Analog:
		visibility.showAnalogHands = true;
		break;
	case EWristwatchDisplayType::Digital:
		visibility.showLcd = true;
		break;
	case EWristwatchDisplayType::Hybrid:
		visibility.showAnalogHands = true;
		visibility.showLcd = surgeMode == EWristwatchSurgeMode::ActiveSurge;
		break;
	}

	return visibility;
}

CWristwatchController::SLcdDigits CWristwatchController::ComputeLcdDigits(
	bool showLcd,
	EWristwatchSurgeMode surgeMode,
	u64 displayGameTime,
	u32 countdownSeconds) const
{
	SLcdDigits digits;
	if (!showLcd)
	{
		return digits;
	}

	if (surgeMode == EWristwatchSurgeMode::ActiveSurge)
	{
		const u32 minutes = countdownSeconds / 60;
		const u32 seconds = countdownSeconds % 60;
		digits.digit0 = minutes / 10;
		digits.digit1 = minutes % 10;
		digits.digit2 = seconds / 10;
		digits.digit3 = seconds % 10;
		return digits;
	}

	u32 year = 0;
	u32 month = 0;
	u32 day = 0;
	u32 hours = 0;
	u32 mins = 0;
	u32 secs = 0;
	u32 milisecs = 0;
	split_time(displayGameTime, year, month, day, hours, mins, secs, milisecs);
	digits.digit0 = hours / 10;
	digits.digit1 = hours % 10;
	digits.digit2 = mins / 10;
	digits.digit3 = mins % 10;
	return digits;
}

void CWristwatchController::ApplyDisplayShadersIfNeeded()
{
	if (_hasAppliedDisplayType && _lastAppliedDisplayType == _settings.displayType)
	{
		return;
	}

	const bool needsShaderPass = IsWristwatchContentConfigured() ||
		_settings.displayType == EWristwatchDisplayType::Analog;

	if (!needsShaderPass)
	{
		return;
	}

	Render->wristwatch_apply_display_shaders(static_cast<u8>(_settings.displayType), g_player_hud->GetWatchesModel());
	_lastAppliedDisplayType = _settings.displayType;
	_hasAppliedDisplayType = true;
}

void CWristwatchController::Update(CActor& actor)
{
	const bool watchesOnHud = g_pGameLevel != nullptr && Level().game != nullptr && g_player_hud != nullptr
		&& g_player_hud->GetWatchesModel() != nullptr;

	if (watchesOnHud != _hudWatchesActive)
	{
		_hudWatchesActive = watchesOnHud;
		if (watchesOnHud)
		{
			_surgeProvider.OnWatchesActive(_settings.replaceSurgeNotifications);
		}
		else
		{
			_surgeProvider.OnWatchesInactive();
		}
	}

	if (!watchesOnHud)
	{
		Device.hudViewportData.wristwatch = {};
		_hasAppliedDisplayType = false;
		_lastWatchesModel = nullptr;
		return;
	}

	IKinematics* watchesModel = g_player_hud->GetWatchesModel();
	if (watchesModel != _lastWatchesModel)
	{
		_hasAppliedDisplayType = false;
		_lastWatchesModel = watchesModel;
	}

	const u64 liveGameTime = Level().GetGameTime();
	const SWristwatchSurgeState surgeState = _surgeProvider.QueryState();
	const EWristwatchSurgeMode surgeMode = ResolveSurgeMode(surgeState);
	const u64 displayGameTime = ResolveDisplayGameTime(liveGameTime, surgeMode);
	const SVisibilityState visibility = ResolveVisibility(surgeMode);
	const SLcdDigits lcdDigits = ComputeLcdDigits(
		visibility.showLcd,
		surgeMode,
		displayGameTime,
		surgeState.countdownSeconds);

	UpdateAnomalyGlitch(actor);

	const SWristwatchRuntimeSettings& runtimeSettings = GetWristwatchRuntimeSettings();
	auto& hudData = Device.hudViewportData.wristwatch;
	hudData.isActive = true;
	hudData.displayGameTime = displayGameTime;
	hudData.countdownSeconds = surgeState.countdownSeconds;
	hudData.displayType = static_cast<u8>(_settings.displayType);
	hudData.surgeMode = static_cast<u8>(surgeMode);
	hudData.showAnalogHands = visibility.showAnalogHands;
	hudData.showLcd = visibility.showLcd;
	hudData.lcdLayout.set(
		runtimeSettings.lcdCenterX,
		runtimeSettings.lcdCenterY,
		runtimeSettings.lcdHalfW,
		runtimeSettings.lcdHalfH);
	hudData.lcdDigits.set(
		static_cast<float>(lcdDigits.digit0) / 10.0f,
		static_cast<float>(lcdDigits.digit1) / 10.0f,
		static_cast<float>(lcdDigits.digit2) / 10.0f,
		static_cast<float>(lcdDigits.digit3) / 10.0f);

	WristwatchFont::UpdateGlyphs(hudData, lcdDigits.digit0, lcdDigits.digit1, lcdDigits.digit2, lcdDigits.digit3);
	ApplyDisplayShadersIfNeeded();

	const float radiation = actor.conditions().GetRadiation();
	hudData.radiationGlow = _settings.radiationGlowMaxMsv > 0.0f
		? clampr(radiation / _settings.radiationGlowMaxMsv, 0.0f, 1.0f)
		: 0.0f;

	hudData.motionIconLuminosity = (g_pMotionIcon != nullptr)
		? g_pMotionIcon->GetLuminosityNormalized()
		: 0.0f;

	ApplyBoneVisibility();
}

void CWristwatchController::UpdateAnomalyGlitch(const CActor& actor)
{
	float glitchStrength = 0.0f;

	if (g_SpatialSpace != nullptr && _settings.anomalyGlitchRadius > 0.0f)
	{
		static xr_vector<ISpatialShared> anomalyObjects;
		anomalyObjects.clear();
		anomalyObjects.reserve(16);

		g_SpatialSpace->q_sphere(
			anomalyObjects,
			0,
			ESPATIAL_TYPE::ANOMALY_ZONE,
			actor.Position(),
			_settings.anomalyGlitchRadius);

		for (const ISpatialShared& spatial : anomalyObjects)
		{
			if (!spatial.get())
			{
				continue;
			}

			const float distance = actor.Position().distance_to(spatial->spatial.sphere.P);
			const float influence = 1.0f - clampr(distance / _settings.anomalyGlitchRadius, 0.0f, 1.0f);
			glitchStrength = std::max(glitchStrength, influence * influence * (3.0f - 2.0f * influence));
		}
	}

	Device.hudViewportData.wristwatch.glitchStrength = glitchStrength;
}

void CWristwatchController::ApplyBoneVisibility() const
{
	const auto& hudData = Device.hudViewportData.wristwatch;
	const bool hideStaticDialForLcd =
		hudData.displayType != static_cast<u8>(EWristwatchDisplayType::Digital) &&
		g_player_hud->HasWatchesLcdSlots() &&
		hudData.showLcd;

	g_player_hud->ApplyWatchesBoneVisibility(
		hudData.showAnalogHands,
		hudData.showLcd,
		hideStaticDialForLcd);
}
