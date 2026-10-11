#pragma once

#include "WatchTypes.h"

struct SWatchGameTime
{
	u32 Year = 0;
	u32 Month = 0;
	u32 Day = 0;
	u32 Hour = 0;
	u32 Minute = 0;
	u32 Second = 0;
};

namespace WatchLogic
{
float SmoothTowards(float Current, float Target, float Dt, float Rate);
float HashUnit(u32 Value);
float EvaluateLagUpdateHz(float BaseHz, float MinHz, float Strength);
float DisplayFlickerNoise(float Phase);
float CompassChaosSpin(float Time);

void UpdateLedBlink(float& BlinkPhase, float& Value, const SWatchLedBlink& Blink, float Intensity, float Dt);

void ParseFormat(const char* Format, bool Date, xr_vector<SWatchFormatToken>& Out);
void FormatText(const xr_vector<SWatchFormatToken>& Tokens, const SWatchGameTime& Time, string64& Out);

bool ParseLightSchedule(const char* Text, xr_vector<SWatchLightKey>& Keys);
float EvaluateLightSchedule(const xr_vector<SWatchLightKey>& Keys, const SWatchLight& Light, float Hour);

bool IsPointerKind(EWatchCompassSourceKind Kind);

bool WorldDirToCompassAngle(const Fvector& WorldDir, const Fmatrix& View, float OffsetRad, float& OutAngle);

void StepCompassSpring(float& Angle, float& Velocity, float Desired, float Stiffness, float Damping, float Dt);

void MakeOffsetXform(const Fvector& Position, const Fvector& RotationDeg, Fmatrix& Out);
}
