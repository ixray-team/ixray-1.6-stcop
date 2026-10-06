#include "StdAfx.h"
#include "WatchDevice.h"
#include "WatchInternal.h"
#include "Actor.h"
#include "ui/UIMotionIcon.h"
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
}

void CWatchDevice::DrawImGui()
{
	if (!Loaded)
	{
		ImGui::TextUnformatted("Watch device is not loaded");
		return;
	}

	ImGui::Text("Section: %s", Section.c_str());
	ImGui::Checkbox("Enabled", &Config.Root.Enabled);
	ImGui::Text("Bones OK: %s", BonesOk() ? "yes" : "no");
	ImGui::Text(
		"Bones: compass=%d light=%d ui=%d rad=%d anom=%d",
		BoneIds.Compass != BI_NONE ? 1 : 0,
		BoneIds.CompassLight != BI_NONE ? 1 : 0,
		BoneIds.Display != BI_NONE ? 1 : 0,
		BoneIds.RadIndicator != BI_NONE ? 1 : 0,
		BoneIds.AnomIndicator != BI_NONE ? 1 : 0
	);
	ImGui::Text(
		"Angle: %.1f deg | Error: %.1f deg | Target: %s",
		rad2deg(State.CompassAngle),
		rad2deg(State.CompassError),
		State.TargetValid ? "valid" : "none"
	);

	DrawCompassImGui();
	DrawDisplayImGui();
	DrawBarometerImGui();
	DrawLagImGui("surge_lag", "Display Lag (Surge)", Config.SurgeLag, Surge.Lag);
	DrawLagImGui("anomaly_lag", "Display Lag (Anomaly)", Config.AnomalyLag, Surge.AnomalyLag);
	DrawLightImGui();
	for (u32 Index = 0; Index < WatchLedChannelCount; ++Index)
	{
		DrawChannelImGui(EWatchLedChannel(Index));
	}
	DrawPreviewImGui();
	DrawHotSaveImGui();
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
	DrawWatchStringInput("Mode", CompassConfig.Mode, ModeBuffer, sizeof(ModeBuffer));
	DrawWatchStringInput("No Target", CompassConfig.NoTarget, NoTargetBuffer, sizeof(NoTargetBuffer));

	ImGui::DragFloat("Stiffness", &CompassConfig.Stiffness, 0.1f, 0.0f, 100.0f, "%.2f");
	ImGui::DragFloat("Damping", &CompassConfig.Damping, 0.1f, 0.0f, 100.0f, "%.2f");
	ImGui::DragFloat("Gravity Strength", &CompassConfig.GravityStrength, 1.0f, 0.0f, 360.0f, "%.1f");
	ImGui::DragFloat("Angle Offset", &CompassConfig.AngleOffset, 1.0f, -360.0f, 360.0f, "%.1f");
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
	bool TextChanged = DrawWatchStringInput("Time Format", DisplayConfig.TimeFormat, TimeFormat, sizeof(TimeFormat));
	TextChanged |= DrawWatchStringInput("Date Format", DisplayConfig.DateFormat, DateFormat, sizeof(DateFormat));
	TextChanged |= ImGui::DragFloat("Update Hz", &DisplayConfig.UpdateHz, 0.05f, 0.0f, 60.0f, "%.2f");
	ImGui::TextDisabled("Tokens: HH MM SS DD YY YYYY, 0 Hz = every frame");
	if (TextChanged)
	{
		UpdateDisplayFormats();
	}

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

	bool LayoutChanged = ImGui::DragFloat3("UI Position", &DisplayConfig.UiPosition.x, 0.0001f, -1.0f, 1.0f, "%.4f");
	LayoutChanged |= ImGui::DragFloat3("UI Rotation", &DisplayConfig.UiRotation.x, 0.5f, -360.0f, 360.0f, "%.1f");
	LayoutChanged |= ImGui::DragFloat("UI Scale", &DisplayConfig.UiScale, 0.005f, 0.05f, 10.0f, "%.3f");
	ImGui::TextDisabled("Rect: x, y, width, height");
	LayoutChanged |= ImGui::DragFloat4("Time Rect", &DisplayConfig.UiTime.x, 0.0001f, -0.1f, 0.1f, "%.4f");
	LayoutChanged |= ImGui::DragFloat4("Date Rect", &DisplayConfig.UiDate.x, 0.0001f, -0.1f, 0.1f, "%.4f");
	LayoutChanged |= ImGui::DragFloat4("Barometer Rect", &DisplayConfig.UiBarometer.x, 0.0001f, -0.1f, 0.1f, "%.4f");
	LayoutChanged |= ImGui::DragFloat4("Barometer Unit Rect", &DisplayConfig.UiBarometerUnit.x, 0.0001f, -0.1f, 0.1f, "%.4f");
	if (LayoutChanged)
	{
		UpdateUILayout();
	}
}

void CWatchDevice::DrawBarometerImGui()
{
	if (!ImGui::CollapsingHeader("Barometer", ImGuiTreeNodeFlags_DefaultOpen))
	{
		return;
	}

	if (Surge.BridgeMissing)
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
		BoneIds.CompassLight != BI_NONE ? "yes" : "no"
	);

	bool LightChanged = ImGui::DragFloat("Day Emission", &LightConfig.DayEmission, 0.01f, 0.0f, 1.0f, "%.2f");
	LightChanged |= ImGui::DragFloat("Dusk Emission", &LightConfig.DuskEmission, 0.01f, 0.0f, 1.0f, "%.2f");
	LightChanged |= ImGui::DragFloat("Night Emission", &LightConfig.NightEmission, 0.01f, 0.0f, 1.0f, "%.2f");
	LightChanged |= ImGui::DragFloat("Dark Emission", &LightConfig.DarkEmission, 0.01f, 0.0f, 1.0f, "%.2f");
	if (LightChanged)
	{
		Light.Timer = 0.0f;
	}

	string256 Schedule = {};
	DrawWatchStringInput("Schedule", LightConfig.Schedule, Schedule, sizeof(Schedule));
	if (ImGui::IsItemDeactivatedAfterEdit())
	{
		UpdateLightSchedule();
	}
	ImGui::TextDisabled("hour:day|dusk|night|dark, ascending, linear blend between keys");

	bool GlowLayoutChanged = ImGui::DragFloat3("Glow Position", &LightConfig.UiPosition.x, 0.0001f, -0.1f, 0.1f, "%.4f");
	GlowLayoutChanged |= ImGui::DragFloat3("Glow Rotation", &LightConfig.UiRotation.x, 0.5f, -360.0f, 360.0f, "%.1f");
	if (GlowLayoutChanged)
	{
		UpdateUILayout();
	}
	ImGui::DragFloat2("Glow Size", &LightConfig.UiSize.x, 0.0001f, 0.0f, 0.1f, "%.4f");
}

void CWatchDevice::DrawChannelImGui(EWatchLedChannel Channel)
{
	const u32 Index = u32(Channel);
	const WatchDetail::SWatchLedChannelDesc& Desc = WatchDetail::ChannelDesc(Channel);
	SWatchChannelPresent& Present = Config.Present[Index];
	const SWatchLedRuntime& Led = Leds[Index];

	string64 Title = {};
	xr_sprintf(Title, "%c%s Indicator", toupper(Desc.Name[0]), Desc.Name + 1);

	ImGui::PushID(Desc.Name);
	if (ImGui::CollapsingHeader(Title, ImGuiTreeNodeFlags_DefaultOpen))
	{
		float Source = 0.0f;
		switch (Channel)
		{
			case EWatchLedChannel::Anomaly:
				Source = Zones.AnomalyTarget;
				break;
			case EWatchLedChannel::Motion:
				Source = g_pMotionIcon ? g_pMotionIcon->GetThreatNormalized() : 0.0f;
				break;
			case EWatchLedChannel::Noise:
				if (const CActor* Player = Actor())
				{
					Source = Player->m_snd_noise;
				}
				break;
			default:
				break;
		}

		ImGui::Text(
			"Intensity: %.2f | Source: %.2f | LED: %s | Bone: %s | Glass: %s | Light: %s",
			State.Intensity[Index],
			Source,
			Led.Value > 0.0f ? "on" : "off",
			Led.Bone != BI_NONE ? "yes" : "no",
			Led.HiddenBone != BI_NONE ? "yes" : "no",
			Led.Light ? (Led.Light->get_active() ? "on" : "idle") : "none"
		);

		bool MeshChanged = ImGui::Checkbox("Enabled", &Present.Enabled);
		MeshChanged |= ImGui::Checkbox("Present Shader", &Present.PresentShader);
		ImGui::Checkbox("Present UI", &Present.PresentUi);
		MeshChanged |= ImGui::Checkbox("Hide Mesh (glass LED)", &Present.HideMesh);
		if (MeshChanged)
		{
			UpdateLedMeshes();
		}

		bool LayoutChanged = ImGui::DragFloat3("LED Position", &Present.UiPosition.x, 0.0001f, -0.1f, 0.1f, "%.4f");
		LayoutChanged |= ImGui::DragFloat3("LED Rotation", &Present.UiRotation.x, 0.5f, -360.0f, 360.0f, "%.1f");
		if (LayoutChanged)
		{
			UpdateUILayout();
		}
		ImGui::DragFloat2("LED Size", &Present.UiSize.x, 0.0001f, 0.0f, 0.1f, "%.4f");
		ImGui::DragFloat2("Glass Length/Radius", &Present.GlassSize.x, 0.0001f, 0.0f, 0.05f, "%.4f");
		ImGui::DragFloat("Brightness", &Present.Brightness, 0.05f, 0.0f, WatchLedMaxBrightness, "%.2f");
		ImGui::Checkbox("Point Light", &Present.LightEnabled);
		ImGui::DragFloat("Light Range", &Present.LightRange, 0.005f, 0.0f, 2.0f, "%.3f");
		ImGui::DragFloat("Light Brightness", &Present.LightBrightness, 0.05f, 0.0f, WatchDetail::HudLightMaxStrength, "%.2f");

		if (Channel == EWatchLedChannel::Anomaly)
		{
			SWatchAnomalySensor& Sensor = Config.Anomaly;
			ImGui::Text("Exclude: %u", u32(Zones.AnomalyExclude.size()));
			ImGui::DragFloat("Radius (m)", &Sensor.Radius, 0.1f, 0.0f, 200.0f, "%.1f");
			ImGui::Checkbox("Exclude Radiation", &Sensor.ExcludeRadiation);

			string512 Exclude = {};
			DrawWatchStringInput("Exclude (section prefixes)", Sensor.Exclude, Exclude, sizeof(Exclude));
			if (ImGui::IsItemDeactivatedAfterEdit())
			{
				UpdateAnomalyExclude();
			}
		}

		DrawWatchBlinkFields(Config.Blink[Index]);
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

	for (u32 Index = 0; Index < WatchLedChannelCount; ++Index)
	{
		const WatchDetail::SWatchLedChannelDesc& Desc = WatchDetail::LedChannels[Index];
		string64 Label = {};
		string64 SliderLabel = {};
		xr_sprintf(Label, "Preview %c%s", toupper(Desc.Name[0]), Desc.Name + 1);
		xr_sprintf(SliderLabel, "%c%s##Preview", toupper(Desc.Name[0]), Desc.Name + 1);
		ImGui::Checkbox(Label, &Preview.PreviewChannel[Index]);
		ImGui::BeginDisabled(!Preview.PreviewChannel[Index]);
		ImGui::SliderFloat(SliderLabel, &Preview.PreviewChannelValue[Index], 0.0f, 1.0f, "%.2f");
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
