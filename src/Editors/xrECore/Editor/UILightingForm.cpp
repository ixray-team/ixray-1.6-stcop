#include "stdafx.h"

#include "ui_main.h"
#include "UILightingForm.h"
#include "../xrEngine/IGame_Level.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrEngine/Environment.h"
#include "IconsFontAwesome7.h"

extern ENGINE_API bool bIsSndOnRoof;
extern ENGINE_API bool bIsRaindropCollision;
// Sun options of the Level Editor light tool
extern ECORE_API bool  bIsShowSun              = false;
extern ECORE_API bool  bIsUseSunDir            = false;
extern ECORE_API bool  bIsUseHemi              = false;

UILightingForm* UILightingForm::Form = nullptr;

namespace
{
	// ps_r_ssao_mode
	const char* const AmbientOcclusionNames[] = { "Off", "SSAO", "GTAO" };

	const char* const TimePresetNames[] = { ICON_FA_SUN " 06", ICON_FA_SUN " 12", ICON_FA_CLOUD_SUN " 18", ICON_FA_MOON " 00" };
	const float TimePresetHours[] = { 6.f, 12.f, 18.f, 0.f };

	bool IsLit()
	{
		return psDeviceFlags.test(rsEnvironment);
	}

	void SelectWeather(const shared_str& Name)
	{
		CEnvironment& Environment = g_pGamePersistent->Environment();
		psDeviceFlags.set(rsEnvironment, true);
		Environment.SetWeather(Name, true);
		EPrefs->sWeather = Environment.CurrentCycleName;
		EContext.UI->RedrawScene();
	}

	void SetLit(bool Lit)
	{
		if (!Lit)
		{
			psDeviceFlags.set(rsEnvironment, false);
			EContext.UI->RedrawScene();
			return;
		}

		const CEnvironment::EnvsMap& Cycles = g_pGamePersistent->Environment().WeatherCycles;
		if (Cycles.empty())
		{
			return;
		}

		// The last used cycle, or the first one
		auto Cycle = Cycles.find(EPrefs->sWeather);
		SelectWeather(Cycle != Cycles.end() ? Cycle->first : Cycles.begin()->first);
	}

	float GetGameTime()
	{
		return g_pGameLevel ? g_pGameLevel->GetEnvironmentGameDayTimeSec() : g_pGamePersistent->Environment().GetGameTime();
	}

	void SetGameTime(float Time)
	{
		CEnvironment& Environment = g_pGamePersistent->Environment();
		const float TimeFactor = g_pGameLevel ? g_pGameLevel->GetEnvironmentTimeFactor() : Environment.fTimeFactor;

		Environment.Invalidate();
		if (g_pGameLevel)
		{
			g_pGameLevel->SetEnvironmentGameTimeFactor(iFloor(static_cast<u64>(Time) * 1000.f), TimeFactor);
		}
		Environment.SetGameTime(Time, TimeFactor);

		float CurrentWeight;
		Environment.lerp(CurrentWeight);
		EContext.UI->RedrawScene();
	}

	void HandCursor()
	{
		if (ImGui::IsItemHovered())
		{
			ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
		}
	}

	void Tooltip(const char* Text)
	{
		if (ImGui::IsItemHovered(ImGuiHoveredFlags_AllowWhenDisabled | ImGuiHoveredFlags_DelayShort))
		{
			ImGui::SetTooltip("%s", Text);
		}
	}

	// Collapsible section with a key/value table
	bool BeginSection(const char* Title)
	{
		ImGui::SetNextItemOpen(true, ImGuiCond_Once);
		if (!XRay::ImGui::BeginExpand(Title))
		{
			return false;
		}

		if (!XRay::ImGui::BeginTable("##props", 2, ImGuiTableFlags_BordersInner | ImGuiTableFlags_RowBg))
		{
			XRay::ImGui::EndExpand();
			return false;
		}

		const float KeyWidth = ImGui::CalcTextSize("Ambient occlusion").x + ImGui::GetStyle().FramePadding.x * 2.f;
		ImGui::TableSetupColumn("##Key", ImGuiTableColumnFlags_WidthFixed, KeyWidth);
		ImGui::TableSetupColumn("##Value", ImGuiTableColumnFlags_WidthStretch);
		return true;
	}

	void EndSection()
	{
		XRay::ImGui::EndTable();
		XRay::ImGui::EndExpand();
	}

	// Label cell, then the value cell sized for one stretched widget
	void Row(const char* Label, const char* Hint = nullptr)
	{
		XRay::ImGui::TableNextRow();
		XRay::ImGui::TableNextColumn();
		XRay::ImGui::TextFramed(Label);
		if (Hint)
		{
			Tooltip(Hint);
		}
		XRay::ImGui::TableNextColumn();
		// Not -FLT_MIN: xrCore redefines FLT_MIN as -FLT_MAX
		ImGui::SetNextItemWidth(-1.f);
	}

	// Enable/Disable toggle of the property lists
	bool Switch(const char* Id, bool* State)
	{
		ImGui::PushID(Id);
		const bool Changed = XRay::ImGui::ToggleButton(*State ? "Enable" : "Disable", State, { -1.f, XRay::ImGui::GetEditorSize(XRay::ImGui::EEditorSizes::TableRowHeight) });
		ImGui::PopID();
		HandCursor();
		if (Changed)
		{
			EContext.UI->RedrawScene();
		}
		return Changed;
	}

	// Equal toolbar buttons joined into one segmented control; returns the clicked index or -1
	int Segmented(const char* const* Labels, int Count, int Selected)
	{
		const float Width = ImGui::GetContentRegionAvail().x / (float)Count;
		int Clicked = -1;

		ImGui::PushStyleVar(ImGuiStyleVar_ItemSpacing, { 0.f, 0.f });
		for (int Index = 0; Index < Count; ++Index)
		{
			ImDrawFlags Rounding = ImDrawFlags_RoundCornersNone;
			if (Index == 0)
			{
				Rounding = ImDrawFlags_RoundCornersLeft;
			}
			else if (Index == Count - 1)
			{
				Rounding = ImDrawFlags_RoundCornersRight;
			}

			if (Index > 0)
			{
				ImGui::SameLine();
			}

			bool Active = Index == Selected;
			ImGui::PushID(Index);
			if (XRay::ImGui::ToolbarButton("##segment", Labels[Index], &Active, { Width, 0.f }, Rounding))
			{
				Clicked = Index;
			}
			HandCursor();
			ImGui::PopID();
		}
		ImGui::PopStyleVar();

		return Clicked;
	}
}

void UILightingForm::Draw()
{
	DrawViewMode();

	ImGui::BeginDisabled(!IsLit());
	DrawEnvironment();
	DrawAmbient();
	DrawLights();
	DrawRain();
	ImGui::EndDisabled();
}

void UILightingForm::DrawViewMode()
{
	if (XRay::ImGui::BeginDarkChild("##view_mode", { 0.f, 0.f }, ImGuiChildFlags_AutoResizeY))
	{
		bool Lit = IsLit();
		if (XRay::ImGui::ToggleSwitch("ViewMode", &Lit, "Lit", "Unlit"))
		{
			SetLit(Lit);
		}
		Tooltip("Unlit: albedo only. Lit: weather, sun, hemi and placed lights.");

		const char* Hint = IsLit() ? "Game lighting" : "Albedo only";
		ImGui::SameLine();
		ImGui::SetCursorPosX(ImGui::GetCursorPosX() + ImGui::GetContentRegionAvail().x - ImGui::CalcTextSize(Hint).x);
		ImGui::AlignTextToFramePadding();
		ImGui::TextDisabled("%s", Hint);
	}
	XRay::ImGui::EndDarkChild();
}

void UILightingForm::DrawEnvironment()
{
	if (!BeginSection(ICON_FA_CLOUD_SUN " Environment"))
	{
		return;
	}

	CEnvironment& Environment = g_pGamePersistent->Environment();

	Row("Weather", "Weather cycle of the Lit view");
	if (ImGui::BeginCombo("##weather", IsLit() ? Environment.CurrentCycleName.c_str() : "-"))
	{
		for (const auto& [Name, _] : Environment.WeatherCycles)
		{
			const bool Selected = IsLit() && Name == Environment.CurrentCycleName;
			if (ImGui::Selectable(Name.c_str(), Selected))
			{
				SelectWeather(Name);
			}
			HandCursor();

			if (Selected)
			{
				ImGui::SetItemDefaultFocus();
			}
		}
		ImGui::EndCombo();
	}
	HandCursor();

	float Time = GetGameTime();
	u32 Hours, Minutes, Seconds;
	Environment.SplitTime(Time, Hours, Minutes, Seconds);

	string32 TimeText;
	xr_sprintf(TimeText, ICON_FA_CLOCK " %02u:%02u:%02u", Hours, Minutes, Seconds);

	Row("Game time", "Time of day of the weather cycle");
	if (ImGui::SliderFloat("##game_time", &Time, 0.f, DAY_LENGTH, TimeText, ImGuiSliderFlags_NoInput))
	{
		SetGameTime(Time);
	}
	HandCursor();

	Row("Presets", "Jump to a time of day");
	const int Preset = Segmented(TimePresetNames, IM_ARRAYSIZE(TimePresetNames), -1);
	if (Preset >= 0)
	{
		SetGameTime(TimePresetHours[Preset] * 3600.f);
	}

	EndSection();
}

void UILightingForm::DrawAmbient()
{
	if (!BeginSection(ICON_FA_CIRCLE_HALF_STROKE " Ambient"))
	{
		return;
	}

	Row("Hemi", "Sky lighting of the whole location. The compilers bake it per vertex for the game.");
	if (ImGui::SliderFloat("##hemi", &EPrefs->Hemi, 0.f, 1.f, "%.2f", ImGuiSliderFlags_AlwaysClamp))
	{
		EContext.UI->RedrawScene();
	}
	HandCursor();

	Row("Ambient occlusion", "Screen space ambient occlusion of the game renderer");
	const int Selected = (int)std::min(ps_r_ssao_mode, (u32)IM_ARRAYSIZE(AmbientOcclusionNames) - 1);
	const int Mode = Segmented(AmbientOcclusionNames, IM_ARRAYSIZE(AmbientOcclusionNames), Selected);
	if (Mode >= 0)
	{
		ps_r_ssao_mode = (u32)Mode;
		EContext.UI->RedrawScene();
	}

	EndSection();
}

void UILightingForm::DrawLights()
{
	if (!BeginSection(ICON_FA_LIGHTBULB " Placed lights"))
	{
		return;
	}

	Row("Lights", "Render the point and spot lights placed on the level");
	Switch("DynamicLights", &EPrefs->DynamicLights);

	ImGui::BeginDisabled(!EPrefs->DynamicLights);
	Row("Shadows", "Shadows of the placed lights with 'Affect dynamic' set");
	Switch("LightShadows", &EPrefs->LightShadows);

	Row("Distance", "Placed lights farther from the camera are skipped");
	if (ImGui::SliderFloat("##light_distance", &EPrefs->LightDistance, 1.f, 10000.f, "%.0f m", ImGuiSliderFlags_Logarithmic | ImGuiSliderFlags_AlwaysClamp))
	{
		EContext.UI->RedrawScene();
	}
	HandCursor();
	ImGui::EndDisabled();

	EndSection();
}

void UILightingForm::DrawRain()
{
	if (!BeginSection(ICON_FA_CLOUD_RAIN " Rain"))
	{
		return;
	}

	Row("Collision", "Raindrops stop at the geometry above them");
	Switch("RaindropCollision", &bIsRaindropCollision);

	Row("Roof sound", "Sound of rain on the roofs");
	if (Switch("SndOnRoof", &bIsSndOnRoof))
	{
		g_pGamePersistent->Environment().ED_Reload();
	}

	EndSection();
}

void UILightingForm::Update()
{
	if (!Form)
	{
		return;
	}

	if (!Form->IsClosed())
	{
		ImGui::SetNextWindowPos(ImGui::GetWindowViewport()->GetCenter(), ImGuiCond_FirstUseEver, { 0.5f, 0.5f });
		ImGui::SetNextWindowSize({ 380.f, 520.f }, ImGuiCond_FirstUseEver);
		// Stable ID: the dock layout is saved by it
		if (ImGui::Begin(ICON_FA_SUN " Lighting###Lighting", &Form->bOpen))
		{
			Form->Draw();
		}
		ImGui::End();
	}
	else
	{
		xr_delete(Form);
	}
}

void UILightingForm::Show()
{
	if (!Form)
	{
		Form = new UILightingForm();
	}
}
