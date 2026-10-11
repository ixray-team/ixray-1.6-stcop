#include "StdAfx.h"
#include "WatchDevice.h"
#include "WatchInternal.h"
#include <imgui.h>

namespace
{
void DrawWatchLagFields(SWatchLag& Lag)
{
	ImGui::DragFloat("Max Delay (game s)", &Lag.MaxDelay, 1.0f, 0.0f, 7200.0f, "%.0f");
	ImGui::DragFloat("Min Update Hz", &Lag.MinUpdateHz, 0.01f, 0.0f, WatchDetail::DisplayMaxHz, "%.2f");
	ImGui::SliderFloat("Freeze Chance", &Lag.FreezeChance, 0.0f, 1.0f, "%.2f");
	ImGui::SliderFloat("Flicker", &Lag.Flicker, 0.0f, 1.0f, "%.2f");
}

void DrawWatchBlinkFields(SWatchLedBlink& Blink)
{
	ImGui::DragFloat("Threshold", &Blink.Threshold, 0.005f, 0.0f, 1.0f, "%.3f");
	ImGui::DragFloat("Solid Threshold", &Blink.SolidThreshold, 0.005f, 0.0f, 1.0f, "%.3f");
	ImGui::DragFloat("Blink Min Hz", &Blink.BlinkMinHz, 0.05f, 0.0f, 30.0f, "%.2f");
	ImGui::DragFloat("Blink Max Hz", &Blink.BlinkMaxHz, 0.05f, 0.0f, 30.0f, "%.2f");
	ImGui::DragFloat("Blink Duty", &Blink.BlinkDuty, 0.01f, 0.0f, 1.0f, "%.2f");
}

bool DrawWatchStringInput(const char* Label, shared_str& Value, char* Buffer, size_t BufferSize)
{
	xr_strcpy(Buffer, BufferSize, Value.size() ? Value.c_str() : "");
	if (ImGui::InputText(Label, Buffer, BufferSize))
	{
		Value = Buffer;
		return true;
	}
	return false;
}

const char* WatchSourceName(const SWatchSource& Source)
{
	switch (Source.Kind)
	{
		case EWatchSourceKind::Anomaly: return "anomaly";
		case EWatchSourceKind::Motion: return "motion";
		case EWatchSourceKind::Noise: return "noise";
		case EWatchSourceKind::Condition: return WatchDetail::ConditionNames[u32(Source.Condition)];
		case EWatchSourceKind::Script: return "script";
		default: return "none";
	}
}

const char* WatchCompassKindName(EWatchCompassSourceKind Kind)
{
	switch (Kind)
	{
		case EWatchCompassSourceKind::North: return "north";
		case EWatchCompassSourceKind::Story: return "story";
		case EWatchCompassSourceKind::Additional: return "additional";
		case EWatchCompassSourceKind::UserNav: return "nav";
		default: return "none";
	}
}
}

void CWatchDevice::DrawImGui()
{
	if (!Loaded)
	{
		ImGui::TextUnformatted("Watch device is not loaded");
		return;
	}

	ImGui::Text("Section: %s | Profile: %s", Section.c_str(), Profile.size() ? Profile.c_str() : "base");
	ImGui::Checkbox("Enabled", &Config.Root.Enabled);
	ImGui::Text("Bones OK: %s | Visible: %s", BonesOk() ? "yes" : "no", Hidden ? "no" : "yes");
	ImGui::Text(
		"Bones: compass=%d light=%d ui=%d rad=%d anom=%d",
		Rig.Bones().Compass != BI_NONE ? 1 : 0,
		Rig.Bones().CompassLight != BI_NONE ? 1 : 0,
		Rig.Bones().Display != BI_NONE ? 1 : 0,
		Rig.Bones().RadIndicator != BI_NONE ? 1 : 0,
		Rig.Bones().AnomIndicator != BI_NONE ? 1 : 0
	);
	ImGui::Text(
		"Angle: %.1f deg | Error: %.1f deg | Target: %s | Kind: %s",
		rad2deg(State.CompassAngle),
		rad2deg(State.CompassError),
		State.TargetValid ? "valid" : "none",
		WatchCompassKindName(State.CompassSourceKind)
	);

	DrawCompassImGui();
	DrawDisplayImGui();
	DrawBarometerImGui();
	DrawLagImGui("surge_lag", "Display Lag (Surge)", Config.SurgeLag, Surge.Lag);
	DrawLagImGui("anomaly_lag", "Display Lag (Anomaly)", Config.AnomalyLag, Surge.AnomalyLag);
	DrawLightImGui();
	for (u32 Index = 0; Index < Leds.size(); ++Index)
	{
		DrawLedImGui(Index);
	}
	for (u32 Index = 0; Index < Gauges.size(); ++Index)
	{
		DrawGaugeImGui(Index);
	}
	DrawPreviewImGui();
	DrawHotSaveImGui();

	const bool EditingNow = ImGui::IsAnyItemActive();
	if (EditingNow)
	{
		CompileConfig(false);
	}
	else if (Editing)
	{
		CompileConfig(true);
	}
	Editing = EditingNow;
}

void CWatchDevice::DrawCompassImGui()
{
	if (!ImGui::CollapsingHeader("Compass", ImGuiTreeNodeFlags_DefaultOpen))
	{
		return;
	}

	SWatchCompass& CompassConfig = Config.Compass;
	char ModeBuffer[32] = {};
	char NoTargetBuffer[32] = {};
	DrawWatchStringInput("Mode (north|quest|nav)", CompassConfig.Mode, ModeBuffer, sizeof(ModeBuffer));
	DrawWatchStringInput("No Target (hide|north)", CompassConfig.NoTarget, NoTargetBuffer, sizeof(NoTargetBuffer));
	ImGui::DragFloat("Stiffness", &CompassConfig.Stiffness, 0.1f, 0.0f, 100.0f, "%.2f");
	ImGui::DragFloat("Damping", &CompassConfig.Damping, 0.1f, 0.0f, 100.0f, "%.2f");
	ImGui::DragFloat("Gravity Strength", &CompassConfig.GravityStrength, 1.0f, 0.0f, 360.0f, "%.1f");
	ImGui::DragFloat("Angle Offset", &CompassConfig.AngleOffset, 1.0f, -360.0f, 360.0f, "%.1f");
	ImGui::DragFloat("Target Glow", &CompassConfig.TargetGlow, 0.01f, 0.0f, 2.0f, "%.2f");
	ImGui::DragFloat4("North Color RGBA", &CompassConfig.NorthColor.x, 1.0f, 0.0f, 255.0f, "%.0f");
	ImGui::DragFloat4("Story Color RGBA", &CompassConfig.StoryColor.x, 1.0f, 0.0f, 255.0f, "%.0f");
	ImGui::DragFloat4("Additional Color RGBA", &CompassConfig.AdditionalColor.x, 1.0f, 0.0f, 255.0f, "%.0f");
	ImGui::DragFloat4("Nav Color RGBA", &CompassConfig.NavColor.x, 1.0f, 0.0f, 255.0f, "%.0f");
}

void CWatchDevice::DrawDisplayImGui()
{
	if (!ImGui::CollapsingHeader("Display UI", ImGuiTreeNodeFlags_DefaultOpen))
	{
		return;
	}

	ImGui::Text("UI: %s | Time: %s | Date: %s", Ui ? "yes" : "no", State.TimeText.c_str(), State.DateText.c_str());

	SWatchDisplay& DisplayConfig = Config.Display;
	string64 TimeFormat = {};
	string64 DateFormat = {};
	DrawWatchStringInput("Time Format", DisplayConfig.TimeFormat, TimeFormat, sizeof(TimeFormat));
	DrawWatchStringInput("Date Format", DisplayConfig.DateFormat, DateFormat, sizeof(DateFormat));
	ImGui::DragFloat("Update Hz", &DisplayConfig.UpdateHz, 0.05f, 0.0f, 60.0f, "%.2f");
	ImGui::TextDisabled("Tokens: HH MM SS DD YY YYYY, 0 Hz = every frame");

	SWatchFonts& Fonts = Config.Fonts;
	string64 FontTime = {};
	string64 FontDate = {};
	string64 FontBarometer = {};
	string64 FontBarometerUnit = {};
	bool FontsChanged = false;
	DrawWatchStringInput("Time Font", Fonts.Time, FontTime, sizeof(FontTime));
	FontsChanged |= ImGui::IsItemDeactivatedAfterEdit();
	DrawWatchStringInput("Date Font", Fonts.Date, FontDate, sizeof(FontDate));
	FontsChanged |= ImGui::IsItemDeactivatedAfterEdit();
	DrawWatchStringInput("Barometer Font", Fonts.Barometer, FontBarometer, sizeof(FontBarometer));
	FontsChanged |= ImGui::IsItemDeactivatedAfterEdit();
	DrawWatchStringInput("Barometer Unit Font", Fonts.BarometerUnit, FontBarometerUnit, sizeof(FontBarometerUnit));
	FontsChanged |= ImGui::IsItemDeactivatedAfterEdit();
	if (FontsChanged)
	{
		CreateUI();
	}

	ImGui::DragFloat3("UI Position", &DisplayConfig.UiPosition.x, 0.0001f, -1.0f, 1.0f, "%.4f");
	ImGui::DragFloat3("UI Rotation", &DisplayConfig.UiRotation.x, 0.5f, -360.0f, 360.0f, "%.1f");
	ImGui::DragFloat("UI Scale", &DisplayConfig.UiScale, 0.005f, 0.05f, 10.0f, "%.3f");
	ImGui::TextDisabled("Rect: x, y, width, height");
	ImGui::DragFloat4("Time Rect", &DisplayConfig.UiTime.x, 0.0001f, -0.1f, 0.1f, "%.4f");
	ImGui::DragFloat4("Date Rect", &DisplayConfig.UiDate.x, 0.0001f, -0.1f, 0.1f, "%.4f");
	ImGui::DragFloat4("Barometer Rect", &DisplayConfig.UiBarometer.x, 0.0001f, -0.1f, 0.1f, "%.4f");
	ImGui::DragFloat4("Barometer Unit Rect", &DisplayConfig.UiBarometerUnit.x, 0.0001f, -0.1f, 0.1f, "%.4f");
}

void CWatchDevice::DrawBarometerImGui()
{
	if (!ImGui::CollapsingHeader("Barometer", ImGuiTreeNodeFlags_DefaultOpen))
	{
		return;
	}

	if (Env->IsSurgeBridgeMissing())
	{
		ImGui::Text("Bridge: missing | Value: %.2f", State.Barometer);
	}
	else
	{
		ImGui::Text("Surge in: %.1f min | Value: %.2f | Surge: %.2f | Text: %s", Surge.Seconds / 60.0f, State.Barometer, State.SurgeFactor, State.BarometerText.c_str());
	}

	SWatchDisplay& DisplayConfig = Config.Display;
	bool BarometerChanged = ImGui::Checkbox("Barometer Enabled", &DisplayConfig.BarometerEnabled);
	BarometerChanged |= ImGui::DragFloat("Lookahead Far (min)", &DisplayConfig.BarometerLookaheadFar, 1.0f, 0.0f, 1440.0f, "%.1f");
	BarometerChanged |= ImGui::DragFloat("Lookahead Near (min)", &DisplayConfig.BarometerLookaheadNear, 1.0f, 0.0f, 1440.0f, "%.1f");
	BarometerChanged |= ImGui::DragFloat("Normal (hPa)", &DisplayConfig.BarometerNormal, 1.0f, 0.0f, 2000.0f, "%.0f");
	BarometerChanged |= ImGui::DragFloat("Drop (hPa)", &DisplayConfig.BarometerDrop, 1.0f, 0.0f, 500.0f, "%.0f");
	if (BarometerChanged)
	{
		Surge.Timer = 0.0f;
	}

	string64 BarometerUnit = {};
	DrawWatchStringInput("Unit (text or string id)", DisplayConfig.BarometerUnit, BarometerUnit, sizeof(BarometerUnit));
	if (ImGui::IsItemDeactivatedAfterEdit())
	{
		CreateUI();
	}
}

void CWatchDevice::DrawLagImGui(const char* Id, const char* Title, SWatchLag& Lag, float LagValue)
{
	ImGui::PushID(Id);
	if (ImGui::CollapsingHeader(Title, ImGuiTreeNodeFlags_DefaultOpen))
	{
		ImGui::Text(
			"Lag: %.2f | Quality: %.2f | Hz: %.2f | Delay: %.0f s | Freeze: %.2f s | Chance: %.2f | Intensity: %.2f | Glitched: %s",
			LagValue,
			State.DisplayQuality,
			Display.Hz,
			Display.Delay,
			Display.Freeze,
			Display.FreezeChance,
			Display.Intensity,
			State.DisplayGlitched ? "yes" : "no"
		);
		DrawWatchLagFields(Lag);
	}
	ImGui::PopID();
}

void CWatchDevice::DrawLightImGui()
{
	if (!ImGui::CollapsingHeader("Light", ImGuiTreeNodeFlags_DefaultOpen))
	{
		return;
	}

	SWatchLight& LightConfig = Config.Light;
	ImGui::Text(
		"Emission: %.2f | Keys: %u | Bone: %s",
		State.Emission,
		u32(Light.Keys.size()),
		Rig.HasCompassLight() ? "yes" : "no"
	);

	ImGui::DragFloat("Day Emission", &LightConfig.DayEmission, 0.01f, 0.0f, 1.0f, "%.2f");
	ImGui::DragFloat("Dusk Emission", &LightConfig.DuskEmission, 0.01f, 0.0f, 1.0f, "%.2f");
	ImGui::DragFloat("Night Emission", &LightConfig.NightEmission, 0.01f, 0.0f, 1.0f, "%.2f");
	ImGui::DragFloat("Dark Emission", &LightConfig.DarkEmission, 0.01f, 0.0f, 1.0f, "%.2f");

	string256 Schedule = {};
	DrawWatchStringInput("Schedule", LightConfig.Schedule, Schedule, sizeof(Schedule));
	if (ImGui::IsItemDeactivatedAfterEdit())
	{
		UpdateLightSchedule(true);
	}
	ImGui::TextDisabled("hour:day|dusk|night|dark, ascending, linear blend between keys");

	ImGui::DragFloat3("Glow Position", &LightConfig.UiPosition.x, 0.0001f, -0.1f, 0.1f, "%.4f");
	ImGui::DragFloat3("Glow Rotation", &LightConfig.UiRotation.x, 0.5f, -360.0f, 360.0f, "%.1f");
	ImGui::DragFloat2("Glow Size", &LightConfig.UiSize.x, 0.0001f, 0.0f, 0.1f, "%.4f");
}

void CWatchDevice::DrawLedImGui(u32 Index)
{
	SWatchLedElement& Element = Config.Leds[Index];
	SWatchChannelPresent& Present = Element.Present;
	const SWatchLedRuntime& Led = Leds[Index];

	string64 Title = {};
	xr_sprintf(Title, "Indicator %s", Element.Name.c_str());

	ImGui::PushID(Element.PresentSection.c_str());
	if (ImGui::CollapsingHeader(Title, ImGuiTreeNodeFlags_DefaultOpen))
	{
		ImGui::Text(
			"Intensity: %.2f | Source: %s | LED: %s | Bone: %s | Glass: %s | Light: %s",
			Led.Intensity,
			WatchSourceName(Led.Source),
			Led.Value > 0.0f ? "on" : "off",
			Rig.HasLedBone(Index) ? "yes" : "no",
			Rig.IsLedMeshHidden(Index) ? "yes" : "no",
			Led.Light ? (Led.Light->get_active() ? "on" : "idle") : "none"
		);

		ImGui::Checkbox("Master", &Element.Master);
		ImGui::Checkbox("Enabled", &Present.Enabled);
		ImGui::Checkbox("Present Shader", &Present.PresentShader);
		ImGui::Checkbox("Present UI", &Present.PresentUi);
		ImGui::Checkbox("Hide Mesh (glass LED)", &Present.HideMesh);

		string128 Source = {};
		DrawWatchStringInput("Source", Present.Source, Source, sizeof(Source));
		ImGui::DragFloat("Smoothing", &Present.Smoothing, 0.1f, 0.0f, 60.0f, "%.2f");

		ImGui::DragFloat3("LED Position", &Present.UiPosition.x, 0.0001f, -0.1f, 0.1f, "%.4f");
		ImGui::DragFloat3("LED Rotation", &Present.UiRotation.x, 0.5f, -360.0f, 360.0f, "%.1f");
		ImGui::DragFloat2("LED Size", &Present.UiSize.x, 0.0001f, 0.0f, 0.1f, "%.4f");
		ImGui::DragFloat2("Glass Length/Radius", &Present.GlassSize.x, 0.0001f, 0.0f, 0.05f, "%.4f");
		ImGui::DragFloat("Brightness", &Present.Brightness, 0.05f, 0.0f, WatchLedMaxBrightness, "%.2f");
		ImGui::Checkbox("Point Light", &Present.LightEnabled);
		ImGui::DragFloat("Light Range", &Present.LightRange, 0.005f, 0.0f, 2.0f, "%.3f");
		ImGui::DragFloat("Light Brightness", &Present.LightBrightness, 0.05f, 0.0f, WatchDetail::HudLightMaxStrength, "%.2f");

		if (Led.Source.Kind == EWatchSourceKind::Anomaly)
		{
			SWatchAnomalySensor& Sensor = Config.Anomaly;
			ImGui::Text("Exclude: %u", u32(Zones.AnomalyExclude.size()));
			ImGui::DragFloat("Radius (m)", &Sensor.Radius, 0.1f, 0.0f, 200.0f, "%.1f");
			ImGui::Checkbox("Exclude Radiation", &Sensor.ExcludeRadiation);

			string512 Exclude = {};
			DrawWatchStringInput("Exclude (section prefixes)", Sensor.Exclude, Exclude, sizeof(Exclude));
		}

		DrawWatchBlinkFields(Element.Blink);
	}
	ImGui::PopID();
}

void CWatchDevice::DrawGaugeImGui(u32 Index)
{
	SWatchGaugeElement& Element = Config.Gauges[Index];
	SWatchConditionPresent& Present = Element.Present;
	const SWatchGaugeRuntime& Gauge = Gauges[Index];

	string64 Title = {};
	xr_sprintf(Title, "Condition %s", Element.Name.c_str());

	ImGui::PushID(Element.PresentSection.c_str());
	if (ImGui::CollapsingHeader(Title))
	{
		ImGui::Text(
			"Raw: %.2f | Display: %.2f | Badness: %.2f | Severity: %s | Visible: %s | Active: %s | Source: %s | Icon: %s | Bar: %s",
			Gauge.Value,
			Gauge.DisplayValue,
			Gauge.Badness,
			WatchDetail::ConditionSeverityName(Gauge.Severity),
			Gauge.Visible ? "yes" : "no",
			IsGaugeActive(Index) ? "yes" : "no",
			WatchSourceName(Gauge.Source),
			Ui && Ui->HasGaugeIcon(Index) ? "yes" : "no",
			Ui && Ui->HasGaugeBar(Index) ? "yes" : "no"
		);

		ImGui::Checkbox("Master", &Element.Master);
		ImGui::Checkbox("Enabled", &Present.Enabled);
		ImGui::Checkbox("Present UI", &Present.PresentUi);
		ImGui::Checkbox("Present Icon", &Present.PresentIcon);
		ImGui::Checkbox("Present Bar", &Present.PresentBar);
		ImGui::Checkbox("Invert", &Present.Invert);
		ImGui::Checkbox("Severity Higher", &Present.SeverityHigher);

		string128 Source = {};
		DrawWatchStringInput("Source", Present.Source, Source, sizeof(Source));
		ImGui::DragFloat("Smoothing", &Present.Smoothing, 0.1f, 0.0f, 60.0f, "%.2f");

		ImGui::DragFloat3("Icon Position", &Present.UiIconPosition.x, 0.0001f, -0.1f, 0.1f, "%.4f");
		ImGui::DragFloat3("Icon Rotation", &Present.UiIconRotation.x, 0.5f, -360.0f, 360.0f, "%.1f");
		ImGui::DragFloat2("Icon Size", &Present.UiIconSize.x, 0.0001f, 0.0f, 0.1f, "%.4f");
		ImGui::DragFloat3("Bar Position", &Present.UiBarPosition.x, 0.0001f, -0.1f, 0.1f, "%.4f");
		ImGui::DragFloat3("Bar Rotation", &Present.UiBarRotation.x, 0.5f, -360.0f, 360.0f, "%.1f");
		ImGui::DragFloat2("Bar Size", &Present.UiBarSize.x, 0.0001f, 0.0f, 0.1f, "%.4f");

		ImGui::DragFloat("Visible Min", &Present.VisibleMin, 0.005f, 0.0f, 1.0f, "%.3f");
		ImGui::DragFloat("Visible Max", &Present.VisibleMax, 0.005f, 0.0f, 1.0f, "%.3f");
		ImGui::DragFloat("Normalize", &Present.Normalize, 0.005f, 0.0f, 10.0f, "%.3f");
		ImGui::DragFloat("Glow", &Present.Glow, 0.05f, 0.0f, 4.0f, "%.2f");
		ImGui::DragFloat("Tier Weak", &Present.TierWeak, 0.005f, 0.0f, 1.0f, "%.3f");
		ImGui::DragFloat("Tier Medium", &Present.TierMedium, 0.005f, 0.0f, 1.0f, "%.3f");
		ImGui::DragFloat("Tier Critical", &Present.TierCritical, 0.005f, 0.0f, 1.0f, "%.3f");
		ImGui::DragFloat("Glow None", &Present.GlowNone, 0.05f, 0.0f, 4.0f, "%.2f");
		ImGui::DragFloat("Glow Weak", &Present.GlowWeak, 0.05f, 0.0f, 4.0f, "%.2f");
		ImGui::DragFloat("Glow Medium", &Present.GlowMedium, 0.05f, 0.0f, 4.0f, "%.2f");
		ImGui::DragFloat("Glow Critical", &Present.GlowCritical, 0.05f, 0.0f, 4.0f, "%.2f");
		ImGui::DragFloat4("Color Weak RGBA", &Present.ColorWeak.x, 1.0f, 0.0f, 255.0f, "%.0f");
		ImGui::DragFloat4("Color Medium RGBA", &Present.ColorMedium.x, 1.0f, 0.0f, 255.0f, "%.0f");
		ImGui::DragFloat4("Color Critical RGBA", &Present.ColorCritical.x, 1.0f, 0.0f, 255.0f, "%.0f");
	}
	ImGui::PopID();
}

void CWatchDevice::DrawPreviewImGui()
{
	if (!ImGui::CollapsingHeader("Preview / Force", ImGuiTreeNodeFlags_DefaultOpen))
	{
		return;
	}

	ImGui::Checkbox("Force Angle", &Preview.ForceAngle);
	ImGui::BeginDisabled(!Preview.ForceAngle);
	ImGui::DragFloat("Force Angle Deg", &Preview.ForceAngleDeg, 1.0f, -180.0f, 180.0f, "%.1f");
	ImGui::EndDisabled();

	if (ImGui::Button("Force North"))
	{
		Preview.ForceAngle = true;
		Preview.ForceAngleDeg = 0.0f;
	}
	ImGui::SameLine();
	if (ImGui::Button("Clear Force"))
	{
		Preview.ForceAngle = false;
	}

	ImGui::Separator();
	ImGui::Checkbox("Preview Gravity", &Preview.PreviewGravity);
	ImGui::BeginDisabled(!Preview.PreviewGravity);
	ImGui::SliderFloat("Gravity##Preview", &Preview.PreviewGravityValue, 0.0f, 1.0f, "%.2f");
	ImGui::EndDisabled();

	ImGui::Checkbox("Preview Surge", &Preview.PreviewSurge);
	ImGui::BeginDisabled(!Preview.PreviewSurge);
	ImGui::SliderFloat("Surge##Preview", &Preview.PreviewSurgeValue, 0.0f, 1.0f, "%.2f");
	ImGui::EndDisabled();

	for (u32 Index = 0; Index < Leds.size(); ++Index)
	{
		SWatchLedRuntime& Led = Leds[Index];
		const char* Name = Config.Leds[Index].Name.c_str();
		string64 Label = {};
		string64 SliderLabel = {};
		xr_sprintf(Label, "Preview %s", Name);
		xr_sprintf(SliderLabel, "%s##PreviewLed", Name);
		ImGui::Checkbox(Label, &Led.Preview);
		ImGui::BeginDisabled(!Led.Preview);
		ImGui::SliderFloat(SliderLabel, &Led.PreviewValue, 0.0f, 1.0f, "%.2f");
		ImGui::EndDisabled();
	}

	for (u32 Index = 0; Index < Gauges.size(); ++Index)
	{
		SWatchGaugeRuntime& Gauge = Gauges[Index];
		const char* Name = Config.Gauges[Index].Name.c_str();
		string64 Label = {};
		string64 SliderLabel = {};
		xr_sprintf(Label, "Preview Cond %s", Name);
		xr_sprintf(SliderLabel, "Cond %s##PreviewGauge", Name);
		ImGui::Checkbox(Label, &Gauge.Preview);
		ImGui::BeginDisabled(!Gauge.Preview);
		ImGui::SliderFloat(SliderLabel, &Gauge.PreviewValue, 0.0f, 1.0f, "%.2f");
		ImGui::EndDisabled();
	}

	if (ImGui::Checkbox("Preview Emission", &Preview.PreviewEmission))
	{
		Light.Timer = 0.0f;
	}
	ImGui::BeginDisabled(!Preview.PreviewEmission);
	ImGui::SliderFloat("Emission##Preview", &Preview.PreviewEmissionValue, 0.0f, 1.0f, "%.2f");
	ImGui::EndDisabled();
}

void CWatchDevice::DrawHotSaveImGui()
{
	if (!ImGui::CollapsingHeader("HotSave", ImGuiTreeNodeFlags_DefaultOpen))
	{
		return;
	}

	if (ImGui::Button("Save Changed"))
	{
		HotSaveChanged();
	}
	ImGui::SameLine();
	if (ImGui::Button("Save All"))
	{
		HotSaveAll();
	}
	ImGui::SameLine();
	if (ImGui::Button("Revert"))
	{
		Revert();
	}
	ImGui::SameLine();
	if (ImGui::Button("Reload"))
	{
		Reload();
	}

	string1024 DirtyText = {};
	FormatDirtySections(DirtyText);
	ImGui::TextWrapped("Dirty: %s", DirtyText);

	ImGui::TextWrapped("Override: %s", OverridePath);
	if (LastHotSaveStatus[0])
	{
		ImGui::TextWrapped("%s", LastHotSaveStatus);
	}
}
