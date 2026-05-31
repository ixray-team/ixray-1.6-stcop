#include "stdafx.h"
#include "WristwatchSettings.h"

namespace
{
constexpr LPCSTR kSection = "wristwatch_settings";

SWristwatchRuntimeSettings g_wristwatchSettings;
SWristwatchSurgeState g_surgeState;
bool g_wristwatchSettingsLoaded = false;
bool g_hudSessionActive = false;

EWristwatchSurgeMode ClampSurgeMode(u8 mode)
{
	if (mode > static_cast<u8>(EWristwatchSurgeMode::ActiveSurge))
	{
		return EWristwatchSurgeMode::Normal;
	}

	return static_cast<EWristwatchSurgeMode>(mode);
}

EWristwatchDisplayType ToDisplayType(LPCSTR value)
{
	if (value == nullptr || value[0] == '\0')
	{
		return EWristwatchDisplayType::Analog;
	}

	if (xr_strcmp(value, "analog") == 0)
	{
		return EWristwatchDisplayType::Analog;
	}

	if (xr_strcmp(value, "digital") == 0)
	{
		return EWristwatchDisplayType::Digital;
	}

	if (xr_strcmp(value, "hybrid") == 0)
	{
		return EWristwatchDisplayType::Hybrid;
	}

	return EWristwatchDisplayType::Analog;
}

void ReadSharedString(shared_str& dest, LPCSTR key, LPCSTR defaultValue)
{
	dest = READ_IF_EXISTS(pSettings, r_string, kSection, key, defaultValue);
}

void ResolveFontTextureFromConfig(SWristwatchRuntimeSettings& settings)
{
	if (settings.fontTexture.size() > 0)
	{
		return;
	}

	LPCSTR faceName = nullptr;
	if (settings.fontFace.size() > 0)
	{
		faceName = settings.fontFace.c_str();
	}
	else if (settings.fontSection.size() > 0 &&
		pSettings->section_exist(settings.fontSection) &&
		pSettings->line_exist(settings.fontSection, "name"))
	{
		faceName = pSettings->r_string(settings.fontSection, "name");
	}

	if (faceName == nullptr || faceName[0] == '\0')
	{
		return;
	}

	string_path textureName;
	xr_sprintf(textureName, "$user$%s", faceName);
	settings.fontTexture = textureName;
}

void UpdateContentReady(SWristwatchRuntimeSettings& settings)
{
	settings.contentReady =
		settings.digitalTexture.size() > 0 &&
		settings.shaderDigital.size() > 0 &&
		settings.shaderFallback.size() > 0 &&
		settings.boneUi.size() > 0 &&
		settings.fontTexture.size() > 0;
}

void LoadFromIni(SWristwatchRuntimeSettings& settings)
{
	settings.game.displayType = ToDisplayType(
		READ_IF_EXISTS(pSettings, r_string, kSection, "display_type", ""));
	settings.game.preSurgeWindow = READ_IF_EXISTS(
		pSettings, r_u32, kSection, "pre_surge_window", 0);
	settings.game.radiationGlowMaxMsv = READ_IF_EXISTS(
		pSettings, r_float, kSection, "radiation_glow_max_msv", 0.0f);
	settings.game.anomalyGlitchRadius = READ_IF_EXISTS(
		pSettings, r_float, kSection, "anomaly_glitch_radius", 0.0f);
	settings.game.replaceSurgeNotifications = READ_IF_EXISTS(
		pSettings, r_bool, kSection, "replace_surge_notifications", false);

	settings.lcdCenterX = READ_IF_EXISTS(pSettings, r_float, kSection, "watches_lcd_center_x", 0.0f);
	settings.lcdCenterY = READ_IF_EXISTS(pSettings, r_float, kSection, "watches_lcd_center_y", 0.0f);
	settings.lcdHalfW = READ_IF_EXISTS(pSettings, r_float, kSection, "watches_lcd_half_w", 0.0f);
	settings.lcdHalfH = READ_IF_EXISTS(pSettings, r_float, kSection, "watches_lcd_half_h", 0.0f);

	ReadSharedString(settings.digitalTexture, "watches_digital_texture", "");
	ReadSharedString(settings.glassTexture, "watches_glass_texture", "");
	ReadSharedString(settings.glassBumpTexture, "watches_glass_bump_texture", "");
	ReadSharedString(settings.fontSection, "watches_font_section", "");
	ReadSharedString(settings.fontTexture, "watches_font_texture", "");
	ReadSharedString(settings.fontFace, "watches_font_face", "");

	ReadSharedString(settings.shaderDigital, "watches_shader_digital", "");
	ReadSharedString(settings.shaderGlass, "watches_shader_glass", "");
	ReadSharedString(settings.shaderHidden, "watches_shader_hidden", "");
	ReadSharedString(settings.shaderFallback, "watches_shader_fallback", "");

	ReadSharedString(settings.boneHud, "watches_bone_hud", "");
	ReadSharedString(settings.boneUi, "watches_bone_ui", "");
	ReadSharedString(settings.boneHandsH, "watches_bone_hands_h", "");
	ReadSharedString(settings.boneHandsM, "watches_bone_hands_m", "");
	ReadSharedString(settings.boneHandsS, "watches_bone_hands_s", "");
	ReadSharedString(settings.boneLcdHh, "watches_bone_lcd_hh", "");
	ReadSharedString(settings.boneLcdHl, "watches_bone_lcd_hl", "");
	ReadSharedString(settings.boneLcdMh, "watches_bone_lcd_mh", "");
	ReadSharedString(settings.boneLcdMl, "watches_bone_lcd_ml", "");
	ReadSharedString(settings.boneTritium, "watches_bone_tritium", "");

	ReadSharedString(settings.surgeScript, "watches_surge_script", "");
	ReadSharedString(settings.surgeHooksFn, "watches_surge_hooks_fn", "");
	ReadSharedString(settings.glassMeshSubstr, "watches_glass_mesh_substr", "");

	settings.debugLcdPass = READ_IF_EXISTS(pSettings, r_u8, kSection, "watches_debug_lcd_pass", 0);
	settings.forceSkipGlassDraw = READ_IF_EXISTS(pSettings, r_bool, kSection, "watches_force_skip_glass", true);

	ResolveFontTextureFromConfig(settings);
}
}

ENGINE_API void ReloadWristwatchRuntimeSettings()
{
	g_wristwatchSettings = {};

	if (!pSettings->section_exist(kSection))
	{
		UpdateContentReady(g_wristwatchSettings);
		g_wristwatchSettingsLoaded = true;
		return;
	}

	LoadFromIni(g_wristwatchSettings);
	UpdateContentReady(g_wristwatchSettings);
	g_wristwatchSettingsLoaded = true;
}

ENGINE_API const SWristwatchRuntimeSettings& GetWristwatchRuntimeSettings()
{
	if (!g_wristwatchSettingsLoaded)
	{
		ReloadWristwatchRuntimeSettings();
	}

	return g_wristwatchSettings;
}

ENGINE_API bool IsWristwatchContentConfigured()
{
	return GetWristwatchRuntimeSettings().contentReady;
}

ENGINE_API void SetWristwatchSurgeState(u8 mode, u32 countdownSeconds, u32 untilSurgeSeconds)
{
	g_surgeState.mode = ClampSurgeMode(mode);
	g_surgeState.countdownSeconds = countdownSeconds;
	g_surgeState.untilSurgeSeconds = untilSurgeSeconds;
}

ENGINE_API const SWristwatchSurgeState& GetWristwatchSurgeState()
{
	return g_surgeState;
}

ENGINE_API void SetWristwatchHudSessionActive(const bool active)
{
	g_hudSessionActive = active;
}

ENGINE_API bool IsWristwatchHudSessionActive()
{
	return g_hudSessionActive;
}

ENGINE_API bool IsWristwatchReplaceSurgeActive()
{
	return g_hudSessionActive && GetWristwatchRuntimeSettings().game.replaceSurgeNotifications;
}
