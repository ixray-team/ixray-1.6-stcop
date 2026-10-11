#pragma once

#include "WatchTypes.h"

class IWatchEnvironment
{
public:
	virtual ~IWatchEnvironment() = default;

	virtual bool HasLevel() const = 0;
	virtual u32 FrameId() const = 0;
	virtual u64 GameTimeMs() const = 0;
	virtual const Fmatrix& ViewMatrix() const = 0;

	virtual bool CanSampleZones() const = 0;
	virtual void SampleZones(bool WantGravity, bool WantAnomaly, const SWatchAnomalySensor& Sensor, const xr_vector<xr_string>& Exclude, float& Gravity, float& Anomaly) = 0;

	virtual void SampleCompassTarget(EWatchCompassMode Mode, SWatchCompassSample& Out) = 0;

	virtual bool IsConditionAvailable(EWatchCondition Condition) const = 0;
	virtual float SampleCondition(EWatchCondition Condition, float BleedingNormalize) const = 0;
	virtual float SampleMotion() const = 0;
	virtual float SampleNoise() const = 0;

	virtual bool SampleSurge(float& SecondsToSurge, float& Progress) = 0;
	virtual bool IsSurgeBridgeMissing() const = 0;

	virtual u32 ResolveScriptSource(const char* Function) = 0;
	virtual float SampleScriptSource(u32 Handle) = 0;
};

xr_unique_ptr<IWatchEnvironment> CreateGameWatchEnvironment();
