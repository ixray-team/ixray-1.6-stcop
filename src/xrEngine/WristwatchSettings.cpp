#include "stdafx.h"
#include "WristwatchSettings.h"

namespace
{
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
	if (value != nullptr)
	{
		if (xr_strcmp(value, "analog") == 0)
		{
			return EWristwatchDisplayType::Analog;
		}

		if (xr_strcmp(value, "digital") == 0)
		{
			return EWristwatchDisplayType::Digital;
		}
	}

	return EWristwatchDisplayType::Hybrid;
}

shared_str BuildDefaultFontTexture(LPCSTR fontSection)
{
	LPCSTR faceName = "digital-7";
	if (pSettings->section_exist(fontSection) && pSettings->line_exist(fontSection, "name"))
	{
		faceName = pSettings->r_string(fontSection, "name");
	}

	string_path textureName;
	xr_sprintf(textureName, "$user$%s", faceName);
	return textureName;
}
}

ENGINE_API void ReloadWristwatchRuntimeSettings()
{
	g_wristwatchSettings = {};
	g_wristwatchSettings.digitalTexture = "secret_hand_textures\\watches\\watches";
	g_wristwatchSettings.glassTexture = "secret_hand_textures\\watches\\watches_glass";
	g_wristwatchSettings.glassBumpTexture = "secret_hand_textures\\watches\\watches_glass_bump#";
	g_wristwatchSettings.fontSection = "font_wristwatch_digital";
	g_wristwatchSettings.fontTexture = BuildDefaultFontTexture(g_wristwatchSettings.fontSection.c_str());

	if (!pSettings->section_exist("wristwatch_settings"))
	{
		g_wristwatchSettingsLoaded = true;
		return;
	}

	const LPCSTR displayType = READ_IF_EXISTS(pSettings, r_string, "wristwatch_settings", "display_type", "hybrid");
	g_wristwatchSettings.game.displayType = ToDisplayType(displayType);
	g_wristwatchSettings.game.preSurgeWindow = READ_IF_EXISTS(pSettings, r_u32, "wristwatch_settings", "pre_surge_window", 600);
	g_wristwatchSettings.game.radiationGlowMaxMsv = READ_IF_EXISTS(pSettings, r_float, "wristwatch_settings", "radiation_glow_max_msv", 0.15f);
	g_wristwatchSettings.game.anomalyGlitchRadius = READ_IF_EXISTS(pSettings, r_float, "wristwatch_settings", "anomaly_glitch_radius", 8.0f);
	g_wristwatchSettings.game.replaceSurgeNotifications = READ_IF_EXISTS(
		pSettings,
		r_bool,
		"wristwatch_settings",
		"replace_surge_notifications",
		false);

	g_wristwatchSettings.lcdCenterX = READ_IF_EXISTS(pSettings, r_float, "wristwatch_settings", "watches_lcd_center_x", g_wristwatchSettings.lcdCenterX);
	g_wristwatchSettings.lcdCenterY = READ_IF_EXISTS(pSettings, r_float, "wristwatch_settings", "watches_lcd_center_y", g_wristwatchSettings.lcdCenterY);
	g_wristwatchSettings.lcdHalfW = READ_IF_EXISTS(pSettings, r_float, "wristwatch_settings", "watches_lcd_half_w", g_wristwatchSettings.lcdHalfW);
	g_wristwatchSettings.lcdHalfH = READ_IF_EXISTS(pSettings, r_float, "wristwatch_settings", "watches_lcd_half_h", g_wristwatchSettings.lcdHalfH);

	if (pSettings->line_exist("wristwatch_settings", "watches_digital_texture"))
	{
		g_wristwatchSettings.digitalTexture = pSettings->r_string("wristwatch_settings", "watches_digital_texture");
	}

	if (pSettings->line_exist("wristwatch_settings", "watches_glass_texture"))
	{
		g_wristwatchSettings.glassTexture = pSettings->r_string("wristwatch_settings", "watches_glass_texture");
	}

	if (pSettings->line_exist("wristwatch_settings", "watches_glass_bump_texture"))
	{
		g_wristwatchSettings.glassBumpTexture = pSettings->r_string("wristwatch_settings", "watches_glass_bump_texture");
	}

	if (pSettings->line_exist("wristwatch_settings", "watches_font_section"))
	{
		g_wristwatchSettings.fontSection = pSettings->r_string("wristwatch_settings", "watches_font_section");
	}

	if (pSettings->line_exist("wristwatch_settings", "watches_font_texture"))
	{
		g_wristwatchSettings.fontTexture = pSettings->r_string("wristwatch_settings", "watches_font_texture");
	}
	else
	{
		g_wristwatchSettings.fontTexture = BuildDefaultFontTexture(g_wristwatchSettings.fontSection.c_str());
	}

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
