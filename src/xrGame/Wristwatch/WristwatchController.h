#pragma once

#include "WristwatchSurgeProvider.h"
#include "WristwatchTypes.h"

class CActor;
class IKinematics;

class CWristwatchController
{
public:
	void LoadSettings();
	void Update(CActor& actor);

private:
	struct SVisibilityState
	{
		bool showAnalogHands = false;
		bool showLcd = false;
	};

	struct SLcdDigits
	{
		u32 digit0 = 0;
		u32 digit1 = 0;
		u32 digit2 = 0;
		u32 digit3 = 0;
	};

	EWristwatchSurgeMode ResolveSurgeMode(const SWristwatchSurgeState& surgeState) const;
	u64 ResolveDisplayGameTime(u64 liveGameTime, EWristwatchSurgeMode surgeMode);
	SVisibilityState ResolveVisibility(EWristwatchSurgeMode surgeMode) const;
	SLcdDigits ComputeLcdDigits(bool showLcd, EWristwatchSurgeMode surgeMode, u64 displayGameTime, u32 countdownSeconds) const;
	void ApplyDisplayShadersIfNeeded();
	void UpdateAnomalyGlitch(const CActor& actor);
	void ApplyBoneVisibility() const;

	SWristwatchSettings _settings;
	CWristwatchSurgeProvider _surgeProvider;
	u64 _frozenGameTime = 0;
	bool _hasFrozenTime = false;
	EWristwatchDisplayType _lastAppliedDisplayType = EWristwatchDisplayType::Hybrid;
	bool _hasAppliedDisplayType = false;
	bool _hudWatchesActive = false;
	IKinematics* _lastWatchesModel = nullptr;
};
