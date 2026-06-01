#pragma once

#include "../xrCore/xrCore.h"

enum class EWristwatchDisplayType : u8
{
	Analog = 0,
	Digital = 1,
	Hybrid = 2,
};

enum class EWristwatchSurgeMode : u8
{
	Normal = 0,
	PreSurgeFreeze = 1,
	ActiveSurge = 2,
};

struct SWristwatchSettings
{
	EWristwatchDisplayType displayType = EWristwatchDisplayType::Hybrid;
	u32 preSurgeWindow = 600;
	float radiationGlowMaxMsv = 0.15f;
	float anomalyGlitchRadius = 8.0f;
};

struct SWristwatchSurgeState
{
	EWristwatchSurgeMode mode = EWristwatchSurgeMode::Normal;
	u32 countdownSeconds = 0;
	u32 untilSurgeSeconds = 0;
};

struct SWristwatchRuntimeSettings
{
	SWristwatchSettings game;
	float lcdCenterX = 0.5f;
	float lcdCenterY = 0.5f;
	float lcdHalfW = 0.128f;
	float lcdHalfH = 0.078f;
	shared_str digitalTexture;
	shared_str glassTexture;
	shared_str fontSection;
	shared_str fontTexture;
};
