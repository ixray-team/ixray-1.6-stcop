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
	EWristwatchDisplayType displayType = EWristwatchDisplayType::Analog;
	u32 preSurgeWindow = 0;
	float radiationGlowMaxMsv = 0.0f;
	float anomalyGlitchRadius = 0.0f;
	bool replaceSurgeNotifications = false;
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
	float lcdCenterX = 0.0f;
	float lcdCenterY = 0.0f;
	float lcdHalfW = 0.0f;
	float lcdHalfH = 0.0f;

	shared_str digitalTexture;
	shared_str glassTexture;
	shared_str glassBumpTexture;
	shared_str fontSection;
	shared_str fontTexture;
	shared_str fontFace;

	shared_str shaderDigital;
	shared_str shaderGlass;
	shared_str shaderHidden;
	shared_str shaderFallback;

	shared_str boneHud;
	shared_str boneUi;
	shared_str boneHandsH;
	shared_str boneHandsM;
	shared_str boneHandsS;
	shared_str boneLcdHh;
	shared_str boneLcdHl;
	shared_str boneLcdMh;
	shared_str boneLcdMl;
	shared_str boneTritium;

	shared_str surgeScript;
	shared_str surgeHooksFn;
	shared_str glassMeshSubstr;

	u8 debugLcdPass = 0;
	bool forceSkipGlassDraw = true;

	bool contentReady = false;
};
