#include "StdAfx.h"
#include "WatchDevice.h"
#include "WatchInternal.h"
#include <imgui.h>

namespace
{
constexpr float WatchDragPosition = 0.0001f;
const ImVec4 WatchWarningColor(1.0f, 0.65f, 0.25f, 1.0f);
const ImVec4 WatchDirtyColor(1.0f, 0.85f, 0.35f, 1.0f);

const char* const WatchSourceItems[] =
	{
		"anomaly",
		"motion",
		"noise",
		"health",
		"power",
		"radiation",
		"satiety",
		"thirst",
		"sleepiness",
		"intoxication",
		"bleeding",
		"script:"
};

const char* const WatchCompassModes[] = {"north", "quest", "nav"};
const char* const WatchCompassNoTargets[] = {"hide", "north"};
const char* const WatchNavigationTargets[] = {"quest", "nav"};

bool DrawWatchText(const char* Label, shared_str& Value)
{
	string512 Buffer = {};
	xr_strcpy(Buffer, Value.size() ? Value.c_str() : "");
	if (ImGui::InputText(Label, Buffer, sizeof(Buffer)))
	{
		Value = Buffer;
		return true;
	}
	return false;
}

template <size_t TCount>
bool DrawWatchChoice(const char* Label, shared_str& Value, const char* const (&Items)[TCount])
{
	bool Changed = false;
	if (ImGui::BeginCombo(Label, Value.size() ? Value.c_str() : ""))
	{
		for (const char* Item : Items)
		{
			const bool Selected = Value.equal(Item);
			if (ImGui::Selectable(Item, Selected))
			{
				Value = Item;
				Changed = true;
			}
		}
		ImGui::EndCombo();
	}
	return Changed;
}

bool IsWatchScriptSource(const shared_str& Value)
{
	const size_t Length = xr_strlen(WatchDetail::ScriptSourcePrefix);
	return Value.size() && !strncmp(Value.c_str(), WatchDetail::ScriptSourcePrefix, Length);
}

void DrawWatchSource(shared_str& Value, const shared_str& Fallback)
{
	const shared_str& Current = Value.size() ? Value : Fallback;
	if (ImGui::BeginCombo("Source", Current.size() ? Current.c_str() : ""))
	{
		for (const char* Item : WatchSourceItems)
		{
			const bool Selected = Current.equal(Item) || (IsWatchScriptSource(Current) && !xr_strcmp(Item, WatchDetail::ScriptSourcePrefix));
			if (ImGui::Selectable(Item, Selected))
			{
				Value = Item;
			}
		}
		ImGui::EndCombo();
	}

	if (IsWatchScriptSource(Value))
	{
		DrawWatchText("Function", Value);
		ImGui::SetItemTooltip("script:<module>.<function>, the function returns 0..1");
	}
}

bool DrawWatchColor(const char* Label, Fvector4& Rgba)
{
	float Color[4] = {Rgba.x / 255.0f, Rgba.y / 255.0f, Rgba.z / 255.0f, Rgba.w / 255.0f};
	if (!ImGui::ColorEdit4(Label, Color, ImGuiColorEditFlags_AlphaBar))
	{
		return false;
	}
	Rgba.set(Color[0] * 255.0f, Color[1] * 255.0f, Color[2] * 255.0f, Color[3] * 255.0f);
	return true;
}

void DrawWatchPreview(const char* Label, bool& Enabled, float& Value, float Min = 0.0f, float Max = 1.0f)
{
	ImGui::PushID(Label);
	ImGui::Checkbox("##Enabled", &Enabled);
	ImGui::SetItemTooltip("Override the live value");
	ImGui::SameLine();
	ImGui::BeginDisabled(!Enabled);
	ImGui::SliderFloat(Label, &Value, Min, Max, "%.2f");
	ImGui::EndDisabled();
	ImGui::PopID();
}

void DrawWatchOffset(Fvector& Position, Fvector* Rotation, Fvector2* Size)
{
	ImGui::DragFloat3("Position", &Position.x, WatchDragPosition, -0.1f, 0.1f, "%.4f");
	if (Rotation)
	{
		ImGui::DragFloat3("Rotation", &Rotation->x, 0.5f, -360.0f, 360.0f, "%.1f");
	}
	if (Size)
	{
		ImGui::DragFloat2("Size", &Size->x, WatchDragPosition, 0.0f, 0.1f, "%.4f");
	}
}

bool DrawWatchTextLayout(const char* Id, shared_str& Font, Fvector4& Rect)
{
	ImGui::PushID(Id);
	DrawWatchText("Font", Font);
	const bool FontChanged = ImGui::IsItemDeactivatedAfterEdit();
	ImGui::DragFloat4("Rect x y w h", &Rect.x, WatchDragPosition, -0.1f, 0.1f, "%.4f");
	ImGui::PopID();
	return FontChanged;
}

void DrawWatchWarning(const char* Text)
{
	ImGui::TextColored(WatchWarningColor, "%s", Text);
}

bool IsWatchLagActive(const SWatchLag& Lag)
{
	return Lag.MaxDelay > 0.0f || Lag.FreezeChance > 0.0f || Lag.Flicker > 0.0f;
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
		default: return "unknown";
	}
}

const char* WatchCompassKindName(EWatchCompassSourceKind Kind)
{
	switch (Kind)
	{
		case EWatchCompassSourceKind::North: return "north";
		case EWatchCompassSourceKind::Story: return "story task";
		case EWatchCompassSourceKind::Additional: return "side task";
		case EWatchCompassSourceKind::UserNav: return "map mark";
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

	ImGui::PushItemWidth(ImGui::GetFontSize() * -11.0f);
	DrawToolbarImGui();
	ImGui::Separator();

	auto LedVisibleLambda = [this](u32 Index)
	{
		return ShowInactive || Leds[Index].Enabled;
	};
	auto GaugeVisibleLambda = [this](u32 Index)
	{
		return ShowInactive || (Gauges[Index].Enabled && Config.Gauges[Index].Present.PresentUi);
	};

	bool HasLeds = false;
	for (u32 Index = 0; Index < Leds.size(); ++Index)
	{
		HasLeds |= LedVisibleLambda(Index);
	}
	bool HasGauges = false;
	for (u32 Index = 0; Index < Gauges.size(); ++Index)
	{
		HasGauges |= GaugeVisibleLambda(Index);
	}

	if (!Config.Root.Enabled && !ShowInactive)
	{
		ImGui::TextDisabled("Watch is disabled");
	}
	else if (ImGui::BeginTabBar("WatchTabs"))
	{
		if ((Rig.HasCompass() || ShowInactive) && ImGui::BeginTabItem("Compass"))
		{
			DrawCompassImGui();
			ImGui::EndTabItem();
		}
		if ((Ui || ShowInactive) && ImGui::BeginTabItem("Display"))
		{
			DrawDisplayImGui();
			ImGui::EndTabItem();
		}
		if (HasLeds && ImGui::BeginTabItem("Indicators"))
		{
			for (u32 Index = 0; Index < Leds.size(); ++Index)
			{
				if (LedVisibleLambda(Index))
				{
					DrawLedImGui(Index);
				}
			}
			ImGui::EndTabItem();
		}
		if (HasGauges && ImGui::BeginTabItem("Conditions"))
		{
			for (u32 Index = 0; Index < Gauges.size(); ++Index)
			{
				if (GaugeVisibleLambda(Index))
				{
					DrawGaugeImGui(Index);
				}
			}
			ImGui::EndTabItem();
		}
		ImGui::EndTabBar();
	}
	ImGui::PopItemWidth();

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

void CWatchDevice::DrawToolbarImGui()
{
	ImGui::Text("%s", Profile.size() ? Profile.c_str() : Section.c_str());
	ImGui::SetItemTooltip("Config profile: %s", Profile.size() ? Profile.c_str() : "base");
	ImGui::SameLine();
	ImGui::TextDisabled("| %s", Hidden ? "hidden" : "on screen");

	if (!Rig.Kinematics())
	{
		DrawWatchWarning("No watch model on the hands");
	}
	else if (!BonesOk())
	{
		const SWatchBoneIds& Ids = Rig.Bones();
		string256 Missing = {};
		auto AppendLambda = [&Missing](u16 Bone, const char* Name)
		{
			if (Bone == BI_NONE)
			{
				xr_strcat(Missing, Missing[0] ? ", " : "");
				xr_strcat(Missing, Name);
			}
		};
		AppendLambda(Ids.Compass, "compass");
		AppendLambda(Ids.CompassLight, "compass_light");
		AppendLambda(Ids.Display, "display");
		AppendLambda(Ids.RadIndicator, "rad_indicator");
		AppendLambda(Ids.AnomIndicator, "anom_indicator");
		ImGui::TextColored(WatchWarningColor, "Missing bones: %s", Missing);
	}

	ImGui::Checkbox("Enabled", &Config.Root.Enabled);
	ImGui::SameLine();
	ImGui::Checkbox("Show inactive", &ShowInactive);
	ImGui::SetItemTooltip("Also show disabled parts and parts the model or watch_hud.xml does not have");

	string1024 DirtyText = {};
	FormatDirtySections(DirtyText);
	const bool Dirty = xr_strcmp(DirtyText, "none") != 0;

	ImGui::BeginDisabled(!Dirty);
	if (ImGui::Button("Save Changed"))
	{
		HotSaveChanged();
	}
	ImGui::EndDisabled();
	ImGui::SetItemTooltip("Write changed values to %s", OverridePath[0] ? OverridePath : "watch_override.ltx");
	ImGui::SameLine();
	if (ImGui::Button("Save All"))
	{
		HotSaveAll();
	}
	ImGui::SetItemTooltip("Write every value to %s", OverridePath[0] ? OverridePath : "watch_override.ltx");
	ImGui::SameLine();
	ImGui::BeginDisabled(!Dirty);
	if (ImGui::Button("Revert"))
	{
		Revert();
	}
	ImGui::EndDisabled();
	ImGui::SameLine();
	if (ImGui::Button("Reload"))
	{
		Reload();
	}
	ImGui::SetItemTooltip("Re-read configs from disk");

	if (Dirty)
	{
		ImGui::TextColored(WatchDirtyColor, "Unsaved: %s", DirtyText);
	}
	else if (LastHotSaveStatus[0])
	{
		ImGui::TextDisabled("%s", LastHotSaveStatus);
	}
}

void CWatchDevice::DrawCompassImGui()
{
	SWatchCompass& CompassConfig = Config.Compass;
	ImGui::TextDisabled(
		"Pointing to: %s | angle %.1f | lag %.1f",
		WatchCompassKindName(State.CompassSourceKind),
		rad2deg(State.CompassAngle),
		rad2deg(State.CompassError)
	);

	DrawWatchChoice("Mode", CompassConfig.Mode, WatchCompassModes);
	ImGui::SetItemTooltip("north: always north | quest: active task | nav: map mark, then task");
	if (CompassSettings.Mode != EWatchCompassMode::North)
	{
		DrawWatchChoice("Without target", CompassConfig.NoTarget, WatchCompassNoTargets);
	}
	ImGui::DragFloat("Stiffness", &CompassConfig.Stiffness, 0.1f, 0.0f, 100.0f, "%.2f");
	ImGui::DragFloat("Damping", &CompassConfig.Damping, 0.1f, 0.0f, 100.0f, "%.2f");
	ImGui::DragFloat("Angle offset", &CompassConfig.AngleOffset, 1.0f, -360.0f, 360.0f, "%.1f");

	ImGui::SeparatorText("Glow");
	ImGui::SliderFloat("Glow with target", &CompassConfig.TargetGlow, 0.0f, 2.0f, "%.2f");
	const bool ShowNorth = CompassSettings.Mode == EWatchCompassMode::North || CompassSettings.NoTarget == EWatchCompassNoTarget::North;
	if (ShowNorth || ShowInactive)
	{
		DrawWatchColor("North", CompassConfig.NorthColor);
	}
	if (CompassSettings.Mode != EWatchCompassMode::North || ShowInactive)
	{
		DrawWatchColor("Story task", CompassConfig.StoryColor);
		DrawWatchColor("Side task", CompassConfig.AdditionalColor);
	}
	if (CompassSettings.Mode == EWatchCompassMode::Nav || ShowInactive)
	{
		DrawWatchColor("Map mark", CompassConfig.NavColor);
	}

	if (CompassSettings.IsChaosEnabled() || ShowInactive)
	{
		ImGui::SeparatorText("Gravity anomalies");
		ImGui::TextDisabled("Field: %.2f", Zones.Gravity);
		ImGui::DragFloat("Spin strength", &CompassConfig.GravityStrength, 1.0f, 0.0f, 360.0f, "%.1f");
		DrawWatchPreview("Field##Gravity", Preview.PreviewGravity, Preview.PreviewGravityValue);
	}

	ImGui::SeparatorText("Test");
	DrawWatchPreview("Needle angle", Preview.ForceAngle, Preview.ForceAngleDeg, -180.0f, 180.0f);

	if (Rig.HasCompassLight() || ShowInactive)
	{
		ImGui::SeparatorText("Backlight");
		DrawLightImGui();
	}
}

void CWatchDevice::DrawLightImGui()
{
	SWatchLight& LightConfig = Config.Light;
	ImGui::TextDisabled("Now: %.2f", State.Emission);

	ImGui::SliderFloat("Day", &LightConfig.DayEmission, 0.0f, 1.0f, "%.2f");
	ImGui::SliderFloat("Dusk", &LightConfig.DuskEmission, 0.0f, 1.0f, "%.2f");
	ImGui::SliderFloat("Night", &LightConfig.NightEmission, 0.0f, 1.0f, "%.2f");
	ImGui::SliderFloat("Dark", &LightConfig.DarkEmission, 0.0f, 1.0f, "%.2f");

	DrawWatchText("Schedule", LightConfig.Schedule);
	if (ImGui::IsItemDeactivatedAfterEdit())
	{
		UpdateLightSchedule(true);
	}
	ImGui::SetItemTooltip("hour:day|dusk|night|dark, ascending");

	if (ImGui::Checkbox("##EmissionPreview", &Preview.PreviewEmission))
	{
		Light.Timer = 0.0f;
	}
	ImGui::SetItemTooltip("Override the live value");
	ImGui::SameLine();
	ImGui::BeginDisabled(!Preview.PreviewEmission);
	ImGui::SliderFloat("Test level", &Preview.PreviewEmissionValue, 0.0f, 1.0f, "%.2f");
	ImGui::EndDisabled();

	if (ImGui::TreeNode("Glow layout"))
	{
		DrawWatchOffset(LightConfig.UiPosition, &LightConfig.UiRotation, &LightConfig.UiSize);
		if (Rig.HasCompassTail() || ShowInactive)
		{
			ImGui::DragFloat3("Tail position", &LightConfig.TailUiPosition.x, WatchDragPosition, -0.1f, 0.1f, "%.4f");
		}
		ImGui::TreePop();
	}
}

void CWatchDevice::DrawDisplayImGui()
{
	if (!Ui)
	{
		DrawWatchWarning("No display: missing display bone, watch_hud.xml or fonts");
		if (!ShowInactive)
		{
			return;
		}
	}

	ImGui::TextDisabled(
		"Quality %.2f | %.1f Hz | brightness %.2f%s",
		State.DisplayQuality,
		Display.Hz,
		Display.Intensity,
		State.DisplayGlitched ? " | glitching" : ""
	);

	SWatchDisplay& DisplayConfig = Config.Display;
	SWatchFonts& Fonts = Config.Fonts;
	bool FontsChanged = false;

	ImGui::DragFloat("Update Hz", &DisplayConfig.UpdateHz, 0.05f, 0.0f, 60.0f, "%.2f");
	ImGui::SetItemTooltip("0 = every frame");

	if ((Ui && Ui->HasTime()) || ShowInactive)
	{
		ImGui::SeparatorText("Time");
		ImGui::PushID("Time");
		DrawWatchText("Format", DisplayConfig.TimeFormat);
		ImGui::SetItemTooltip("HH MM SS");
		if (ImGui::TreeNode("Layout"))
		{
			FontsChanged |= DrawWatchTextLayout("Time", Fonts.Time, DisplayConfig.UiTime);
			ImGui::TreePop();
		}
		ImGui::PopID();
	}

	if ((Ui && Ui->HasDate()) || ShowInactive)
	{
		ImGui::SeparatorText("Date");
		ImGui::PushID("Date");
		DrawWatchText("Format", DisplayConfig.DateFormat);
		ImGui::SetItemTooltip("DD MM YY YYYY");
		if (ImGui::TreeNode("Layout"))
		{
			FontsChanged |= DrawWatchTextLayout("Date", Fonts.Date, DisplayConfig.UiDate);
			ImGui::TreePop();
		}
		ImGui::PopID();
	}

	if ((Config.Navigation.Enabled && Ui && Ui->HasNavigation()) || ShowInactive)
	{
		ImGui::SeparatorText("Navigation");
		FontsChanged |= DrawNavigationImGui();
	}

	if ((DisplayConfig.BarometerEnabled && Ui && Ui->HasBarometer()) || ShowInactive)
	{
		ImGui::SeparatorText("Barometer");
		FontsChanged |= DrawBarometerImGui();
	}

	if (IsWatchLagActive(Config.SurgeLag) || ShowInactive)
	{
		DrawLagImGui("SurgeLag", "Surge interference", Config.SurgeLag, Surge.Lag);
	}
	if ((IsWatchLagActive(Config.AnomalyLag) && UsesSource(EWatchSourceKind::Anomaly)) || ShowInactive)
	{
		DrawLagImGui("AnomalyLag", "Anomaly interference", Config.AnomalyLag, Surge.AnomalyLag);
	}

	ImGui::SeparatorText("Screen");
	if (ImGui::TreeNode("Placement on the model"))
	{
		ImGui::DragFloat3("Position", &DisplayConfig.UiPosition.x, WatchDragPosition, -1.0f, 1.0f, "%.4f");
		ImGui::DragFloat3("Rotation", &DisplayConfig.UiRotation.x, 0.5f, -360.0f, 360.0f, "%.1f");
		ImGui::DragFloat("Scale", &DisplayConfig.UiScale, 0.005f, 0.05f, 10.0f, "%.3f");
		ImGui::TreePop();
	}

	if (FontsChanged)
	{
		CreateUI();
	}
}

bool CWatchDevice::DrawNavigationImGui()
{
	ImGui::PushID("Navigation");
	if (Navigation.Valid)
	{
		ImGui::TextDisabled("%.0f m | arrow %.0f", Navigation.Distance, rad2deg(Navigation.Angle));
	}
	else
	{
		ImGui::TextDisabled("No target on this level");
	}

	SWatchNavigation& NavConfig = Config.Navigation;
	ImGui::Checkbox("Enabled", &NavConfig.Enabled);
	DrawWatchChoice("Target", NavConfig.Target, WatchNavigationTargets);
	ImGui::SetItemTooltip("quest: active task | nav: map mark, then task");
	DrawWatchColor("Arrow color", NavConfig.ArrowColor);
	ImGui::DragFloat("Hide arrow closer than", &NavConfig.NearRadius, 0.1f, 0.0f, 100.0f, "%.1f m");

	bool FontsChanged = false;
	if (ImGui::TreeNode("Units"))
	{
		ImGui::DragFloat("Kilometers from", &NavConfig.KmThreshold, 10.0f, 0.0f, 100000.0f, "%.0f m");
		int Decimals = iFloor(NavConfig.KmDecimals + 0.5f);
		if (ImGui::SliderInt("Kilometer decimals", &Decimals, 0, 3))
		{
			NavConfig.KmDecimals = float(Decimals);
		}
		DrawWatchText("Meters", NavConfig.UnitMeters);
		DrawWatchText("Kilometers", NavConfig.UnitKilometers);
		ImGui::TreePop();
	}

	if (ImGui::TreeNode("Layout"))
	{
		SWatchDisplay& DisplayConfig = Config.Display;
		SWatchFonts& Fonts = Config.Fonts;
		DrawWatchText("Digits font", Fonts.Distance);
		FontsChanged |= ImGui::IsItemDeactivatedAfterEdit();
		DrawWatchText("Unit font", Fonts.DistanceUnit);
		FontsChanged |= ImGui::IsItemDeactivatedAfterEdit();
		ImGui::DragFloat2("Center", &DisplayConfig.UiNav.x, WatchDragPosition, -0.1f, 0.1f, "%.4f");
		ImGui::DragFloat("Digit height", &DisplayConfig.UiNav.z, WatchDragPosition, 0.0f, 0.1f, "%.4f");
		ImGui::DragFloat("Gap", &DisplayConfig.UiNav.w, WatchDragPosition, 0.0f, 0.1f, "%.4f");
		ImGui::DragFloat2("Unit height, shift", &DisplayConfig.UiNavUnit.x, WatchDragPosition, -0.1f, 0.1f, "%.4f");
		ImGui::DragFloat2("Arrow size, shift", &DisplayConfig.UiNavArrow.x, WatchDragPosition, -0.1f, 0.1f, "%.4f");
		ImGui::TreePop();
	}
	ImGui::PopID();
	return FontsChanged;
}

bool CWatchDevice::DrawBarometerImGui()
{
	ImGui::PushID("Barometer");
	SWatchDisplay& DisplayConfig = Config.Display;
	if (Env->IsSurgeBridgeMissing())
	{
		DrawWatchWarning("No surge data: watch_surge.script is missing");
	}
	else if (Surge.Seconds >= 0.0f)
	{
		ImGui::TextDisabled("Surge in %.0f min | %s", Surge.Seconds / 60.0f, State.BarometerText.c_str());
	}
	else
	{
		ImGui::TextDisabled("Surge: %s | %s", Surge.Progress >= 0.0f ? "now" : "unknown", State.BarometerText.c_str());
	}

	bool Changed = ImGui::Checkbox("Enabled", &DisplayConfig.BarometerEnabled);
	Changed |= ImGui::DragFloat("Normal", &DisplayConfig.BarometerNormal, 1.0f, 0.0f, 2000.0f, "%.0f");
	Changed |= ImGui::DragFloat("Drop before surge", &DisplayConfig.BarometerDrop, 1.0f, 0.0f, 500.0f, "%.0f");
	Changed |= ImGui::DragFloatRange2("Falls during", &DisplayConfig.BarometerLookaheadNear, &DisplayConfig.BarometerLookaheadFar, 1.0f, 0.0f, 1440.0f, "%.0f min", "%.0f min");
	ImGui::SetItemTooltip("Starts falling at the right value, fully dropped at the left value (game minutes before the surge)");
	if (Changed)
	{
		Surge.Timer = 0.0f;
	}
	DrawWatchPreview("Test surge", Preview.PreviewSurge, Preview.PreviewSurgeValue);

	bool FontsChanged = false;
	if (ImGui::TreeNode("Layout"))
	{
		DrawWatchText("Unit", DisplayConfig.BarometerUnit);
		FontsChanged |= ImGui::IsItemDeactivatedAfterEdit();
		ImGui::SetItemTooltip("Text or string table id");
		FontsChanged |= DrawWatchTextLayout("Digits", Config.Fonts.Barometer, DisplayConfig.UiBarometer);
		ImGui::TextDisabled("Unit");
		FontsChanged |= DrawWatchTextLayout("UnitText", Config.Fonts.BarometerUnit, DisplayConfig.UiBarometerUnit);
		ImGui::TreePop();
	}
	ImGui::PopID();
	return FontsChanged;
}

void CWatchDevice::DrawLagImGui(const char* Id, const char* Title, SWatchLag& Lag, float LagValue)
{
	ImGui::PushID(Id);
	ImGui::SeparatorText(Title);
	ImGui::TextDisabled("Level %.2f | delay %.0f s | frozen %.1f s", LagValue, Display.Delay, Display.Freeze);
	ImGui::DragFloat("Time delay", &Lag.MaxDelay, 1.0f, 0.0f, 7200.0f, "%.0f s");
	ImGui::DragFloat("Slowest update", &Lag.MinUpdateHz, 0.01f, 0.0f, WatchDetail::DisplayMaxHz, "%.2f Hz");
	ImGui::SliderFloat("Freeze chance", &Lag.FreezeChance, 0.0f, 1.0f, "%.2f");
	ImGui::SliderFloat("Flicker", &Lag.Flicker, 0.0f, 1.0f, "%.2f");
	ImGui::PopID();
}

void CWatchDevice::DrawLedImGui(u32 Index)
{
	SWatchLedElement& Element = Config.Leds[Index];
	SWatchChannelPresent& Present = Element.Present;
	SWatchLedRuntime& Led = Leds[Index];

	ImGui::PushID(Element.PresentSection.c_str());
	if (!ImGui::CollapsingHeader(Element.Name.c_str(), ImGuiTreeNodeFlags_DefaultOpen))
	{
		ImGui::PopID();
		return;
	}

	ImGui::TextDisabled("Signal %.2f | %s | source %s", Led.Intensity, Led.Value > 0.0f ? "lit" : "off", WatchSourceName(Led.Source));
	if (Led.Source.Kind == EWatchSourceKind::None)
	{
		DrawWatchWarning("Unknown source");
	}
	if (Present.PresentShader && !Rig.HasLedBone(Index))
	{
		DrawWatchWarning("Bone not found on the model");
	}

	ImGui::Checkbox("Enabled", &Present.Enabled);
	ImGui::SetItemTooltip("Disabled indicators are hidden here unless Show inactive is on");
	if (ShowInactive || !Element.Master)
	{
		ImGui::SameLine();
		ImGui::Checkbox("Master", &Element.Master);
		ImGui::SetItemTooltip("[watch.indicators] %s", Element.MasterKey.c_str());
	}

	DrawWatchSource(Present.Source, Element.Name);
	ImGui::DragFloat("Smoothing", &Present.Smoothing, 0.1f, 0.0f, 60.0f, "%.2f");
	DrawWatchPreview("Test signal", Led.Preview, Led.PreviewValue);

	if (Led.Source.Kind == EWatchSourceKind::Anomaly && ImGui::TreeNode("Anomaly sensor"))
	{
		SWatchAnomalySensor& Sensor = Config.Anomaly;
		ImGui::DragFloat("Radius", &Sensor.Radius, 0.1f, 0.0f, 200.0f, "%.1f m");
		ImGui::Checkbox("Ignore radiation", &Sensor.ExcludeRadiation);
		DrawWatchText("Ignore sections", Sensor.Exclude);
		ImGui::SetItemTooltip("Comma separated parts of zone section names");
		ImGui::TreePop();
	}

	if (ImGui::TreeNode("Blink"))
	{
		SWatchLedBlink& Blink = Element.Blink;
		ImGui::DragFloatRange2("Blinks between", &Blink.Threshold, &Blink.SolidThreshold, 0.005f, 0.0f, 1.0f, "%.3f", "solid %.3f");
		ImGui::DragFloatRange2("Rate", &Blink.BlinkMinHz, &Blink.BlinkMaxHz, 0.05f, 0.0f, 30.0f, "%.2f Hz", "%.2f Hz");
		ImGui::SliderFloat("On time", &Blink.BlinkDuty, 0.0f, 1.0f, "%.2f");
		ImGui::TreePop();
	}

	ImGui::Checkbox("On model", &Present.PresentShader);
	ImGui::SameLine();
	ImGui::Checkbox("On display", &Present.PresentUi);

	if (Present.PresentShader && ImGui::TreeNode("Model look"))
	{
		ImGui::Checkbox("Glass cap", &Present.HideMesh);
		ImGui::SetItemTooltip("Hide the model lamp and draw a glass cap instead");
		if (Present.HideMesh)
		{
			ImGui::DragFloat2("Glass length, radius", &Present.GlassSize.x, WatchDragPosition, 0.0f, 0.05f, "%.4f");
			ImGui::SliderFloat("Brightness", &Present.Brightness, 0.0f, WatchLedMaxBrightness, "%.2f");
		}
		ImGui::Checkbox("Light on the hand", &Present.LightEnabled);
		if (Present.LightEnabled)
		{
			ImGui::DragFloat("Light range", &Present.LightRange, 0.005f, 0.0f, 2.0f, "%.3f");
			ImGui::DragFloat("Light brightness", &Present.LightBrightness, 0.05f, 0.0f, WatchDetail::HudLightMaxStrength, "%.2f");
		}
		ImGui::TreePop();
	}

	if ((Present.PresentShader || Present.PresentUi) && ImGui::TreeNode("Glow layout"))
	{
		DrawWatchOffset(Present.UiPosition, &Present.UiRotation, &Present.UiSize);
		ImGui::TreePop();
	}
	ImGui::PopID();
}

void CWatchDevice::DrawGaugeImGui(u32 Index)
{
	SWatchGaugeElement& Element = Config.Gauges[Index];
	SWatchConditionPresent& Present = Element.Present;
	SWatchGaugeRuntime& Gauge = Gauges[Index];

	ImGui::PushID(Element.PresentSection.c_str());
	if (!ImGui::CollapsingHeader(Element.Name.c_str(), ImGuiTreeNodeFlags_DefaultOpen))
	{
		ImGui::PopID();
		return;
	}

	ImGui::TextDisabled(
		"Value %.2f | %s | %s",
		Gauge.DisplayValue,
		WatchDetail::ConditionSeverityName(Gauge.Severity),
		Gauge.Visible ? "shown" : "hidden"
	);
	if (Gauge.Source.Kind == EWatchSourceKind::None)
	{
		DrawWatchWarning("Unknown source");
	}
	else if (!IsGaugeActive(Index) && Gauge.Enabled)
	{
		DrawWatchWarning("This condition is off in the game settings");
	}
	if (Ui && !Ui->HasGaugeIcon(Index) && !Ui->HasGaugeBar(Index))
	{
		DrawWatchWarning("No icon or bar in watch_hud.xml");
	}

	ImGui::Checkbox("Enabled", &Present.Enabled);
	ImGui::SetItemTooltip("Disabled conditions are hidden here unless Show inactive is on");
	if (ShowInactive || !Element.Master)
	{
		ImGui::SameLine();
		ImGui::Checkbox("Master", &Element.Master);
		ImGui::SetItemTooltip("[watch.conditions] %s", Element.MasterKey.c_str());
	}
	if (ShowInactive || !Present.PresentUi)
	{
		ImGui::SameLine();
		ImGui::Checkbox("Show on display", &Present.PresentUi);
	}

	DrawWatchSource(Present.Source, Element.Name);
	ImGui::DragFloat("Smoothing", &Present.Smoothing, 0.1f, 0.0f, 60.0f, "%.2f");
	if (Gauge.Source.Kind == EWatchSourceKind::Condition && Gauge.Source.Condition == EWatchCondition::Bleeding)
	{
		ImGui::DragFloat("Full scale", &Present.Normalize, 0.005f, 0.0f, 10.0f, "%.3f");
		ImGui::SetItemTooltip("Bleeding speed shown as 100%%");
	}
	DrawWatchPreview("Test value", Gauge.Preview, Gauge.PreviewValue);

	if (ImGui::TreeNode("When to show"))
	{
		ImGui::Checkbox("Invert value", &Present.Invert);
		ImGui::Checkbox("Higher is worse", &Present.SeverityHigher);
		ImGui::DragFloatRange2("Visible range", &Present.VisibleMin, &Present.VisibleMax, 0.005f, 0.0f, 1.0f, "%.3f", "%.3f");
		ImGui::SliderFloat("Weak from", &Present.TierWeak, 0.0f, 1.0f, "%.3f");
		ImGui::SliderFloat("Medium from", &Present.TierMedium, 0.0f, 1.0f, "%.3f");
		ImGui::SliderFloat("Critical from", &Present.TierCritical, 0.0f, 1.0f, "%.3f");
		ImGui::TreePop();
	}

	if (ImGui::TreeNode("Look"))
	{
		ImGui::Checkbox("Icon", &Present.PresentIcon);
		ImGui::SameLine();
		ImGui::Checkbox("Bar", &Present.PresentBar);
		ImGui::SliderFloat("Brightness", &Present.Glow, 0.0f, 4.0f, "%.2f");
		ImGui::SliderFloat("Glow weak", &Present.GlowWeak, 0.0f, 4.0f, "%.2f");
		ImGui::SliderFloat("Glow medium", &Present.GlowMedium, 0.0f, 4.0f, "%.2f");
		ImGui::SliderFloat("Glow critical", &Present.GlowCritical, 0.0f, 4.0f, "%.2f");
		DrawWatchColor("Weak", Present.ColorWeak);
		DrawWatchColor("Medium", Present.ColorMedium);
		DrawWatchColor("Critical", Present.ColorCritical);
		ImGui::TreePop();
	}

	if (ImGui::TreeNode("Layout"))
	{
		if (Present.PresentIcon)
		{
			ImGui::PushID("Icon");
			ImGui::TextDisabled("Icon");
			DrawWatchOffset(Present.UiIconPosition, &Present.UiIconRotation, &Present.UiIconSize);
			ImGui::PopID();
		}
		if (Present.PresentBar)
		{
			ImGui::PushID("Bar");
			ImGui::TextDisabled("Bar");
			DrawWatchOffset(Present.UiBarPosition, &Present.UiBarRotation, &Present.UiBarSize);
			ImGui::PopID();
		}
		ImGui::TreePop();
	}
	ImGui::PopID();
}
