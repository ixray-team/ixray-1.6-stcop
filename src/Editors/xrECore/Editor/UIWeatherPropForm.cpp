#include "stdafx.h"

#include "ui_main.h"
#include "UIWeatherPropForm.h"
#include "engine/xrGamePersistentEditors.h"
#include "../xrEngine/IGame_Level.h"
#include "../xrEngine/IGame_Persistent.h"
#include "../xrEngine/Environment.h"
#include "UI_ToolsCustom.h"
#include "../xrEngine/Editor/XrEditorSceneInterface.h"

// extern ENGINE_API BOOL bIsSndOnRoof;
// extern ENGINE_API BOOL bIsRaindropCollision;
extern ECORE_API bool  bIsShowSun              = false;
extern ECORE_API bool  bIsUseSunDir            = false;
extern ECORE_API bool  bIsUseHemi              = false;
UIWeatherPropForm*     UIWeatherPropForm::Form = nullptr;

UIWeatherPropForm::UIWeatherPropForm()
{
    m_weather_properties = EDevice->Resources->_CreateTexture("ed\\bar\\WeatherProp");
    m_speed_time         = EPrefs->env_speed;
    // m_snd_on_roof        = bIsSndOnRoof;
    // m_raindrop_collision = bIsRaindropCollision;

    m_sun_visible        = false;
    m_use_sun_dir        = false;
    m_use_hemi           = false;
}

UIWeatherPropForm::~UIWeatherPropForm() {}

void UIWeatherPropForm::Draw()
{
    auto&  env         = g_pGamePersistent->Environment();
    float  time        = g_pGameLevel ? g_pGameLevel->GetEnvironmentGameDayTimeSec() : env.GetGameTime();
    float  time_factor = g_pGameLevel ? g_pGameLevel->GetEnvironmentTimeFactor() : env.fTimeFactor;
    ImVec2 sizeImage   = ImVec2(454.0f, 250.0f);   // Размер изображения, которое мы хотим сделать видимым
    ImVec2 sizeButton  = ImVec2(100.0f, 22.0f);    // Размер кнопки, которое мы хотим сделать видимым

    // --------------------------------------------------------------------------------------------
    // Картинка
	{
		ImVec2 image_pos = ImGui::GetCursorScreenPos();
        m_weather_properties->Load();
        ImGui::SameLine(0, 0);
        ImGui::Image(m_weather_properties->pSurface, ImVec2(sizeImage));

		// Переводим секунды в часы (0-24)
		float hours = std::fmod(time / 3600.0f, 24.0f);

		// Диапазон ночи
		float alpha = 0.0f;
		if (hours >= 18.0f)
		{
			alpha = (hours - 18.0f) / 4.0f;
		}
		else if (hours <= 6.0f)
		{
			alpha = 1.0f - (hours / 6.0f);
		}

		// Корректируем диапазон (0 - 1)
		alpha = std::clamp(alpha, 0.0f, 0.85f);

		// Конвертируем в диапазон 0 - 255
		int alpha_int = static_cast<int>(alpha * 255.0f);

		// Накладываем затемнение (если есть)
		if (alpha_int > 0)
		{
			ImDrawList* draw_list = ImGui::GetWindowDrawList();
			draw_list->AddRectFilled(
				image_pos,
				ImVec2(image_pos.x + sizeImage.x, image_pos.y + sizeImage.y),
				ImColor(0, 0, 0, alpha_int)
			);
		}
    }
    // --------------------------------------------------------------------------------------------
    ImGui::BeginChild("Center", ImVec2(455, 575), true);
    // --------------------------------------------------------------------------------------------
    // Установки времени погоды
    {
        pcstr titleTime = "Time settings";
        ImGui::SetCursorPosX((ImGui::GetWindowWidth() - ImGui::CalcTextSize(titleTime).x) / 2);
        ImGui::TextColored(ImVec4(0, 1, 1, 0.7), titleTime);
        // --------------------------------------------------------------------------------------------
        if (ImGui::CollapsingHeader("Environment time", ImGuiTreeNodeFlags_DefaultOpen))
		{
			u32 hours, minutes, seconds;
			env.SplitTime(time, hours, minutes, seconds);

            string128 temp;
            xr_sprintf(temp, "Current time: %02d:%02d:%02d###environment_time", hours, minutes, seconds);

            if (ImGui::SliderFloat(temp, &time, 0, DAY_LENGTH, ""))
            {
                env.Invalidate();
                if (g_pGameLevel)
                    g_pGameLevel->SetEnvironmentGameTimeFactor(iFloor(static_cast<u64>(time) * 1000.f), time_factor);
                env.SetGameTime(time, time_factor);
                float current_weight;
                env.lerp(current_weight);
            }
            if (ImGui::DragFloat("Time factor", &time_factor, 1.f, 1.f, 10000.f))
            {
                if (g_pGameLevel)
                    g_pGameLevel->SetEnvironmentGameTimeFactor(iFloor(static_cast<u64>(time) * 10000.f), time_factor);
                env.SetGameTime(time, time_factor);
            }
            // --------------------------------------------------------------------------------------------
            ImGui::Separator();
            ImGui::Spacing();
            // --------------------------------------------------------------------------------------------
            {
                ImGui::BeginGroup();
                ImGui::SameLine(0, 3);
                ImGui::InputFloat("Speed", &m_speed_time, 1.f, 10000.f, 1.f, 1);
                /*ImGui::SameLine(0);
                if (ImGui::Button("Apply Speed"_RU >> u8"Применить"))
                    env.fTimeFactor = m_speed_time;*/
                ImGui::EndGroup();
            }
        }
    }
    // --------------------------------------------------------------------------------------------
    ImGui::Spacing();
    ImGui::Separator();
    ImGui::Spacing();
    // --------------------------------------------------------------------------------------------
    // Выбор погодного цикла
    bool selectedWeather = psDeviceFlags.test(rsEnvironment);
    {
        pcstr titleWeatherCycle = "Weather cycle selection";
        ImGui::SetCursorPosX((ImGui::GetWindowWidth() - ImGui::CalcTextSize(titleWeatherCycle).x) / 2);
        ImGui::TextColored(ImVec4(0, 1, 1, 0.7), titleWeatherCycle);
        // --------------------------------------------------------------------------------------------
        if (ImGui::BeginCombo("Weather cycle", env.CurrentCycleName.c_str()))
        {
            for (auto& i: env.WeatherCycles)
            {
                selectedWeather = psDeviceFlags.test(rsEnvironment) && i.first == EPrefs->sWeather;
                if (ImGui::MenuItem(i.first.c_str(), "", &selectedWeather))
                {
                    psDeviceFlags.set(rsEnvironment, true);
                    env.SetWeather(i.first.c_str(), true);
                    EPrefs->sWeather = env.CurrentCycleName;
                    UI->RedrawScene();
                }
                if (ImGui::IsItemHovered())
                    ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            }
            ImGui::EndCombo();
        }
    }
    // --------------------------------------------------------------------------------------------
    ImGui::Separator();
    // --------------------------------------------------------------------------------------------
    // Освещение
    {
        pcstr titleLightingSettings = "Lighting settings in the Editor";
        ImGui::SetCursorPosX((ImGui::GetWindowWidth() - ImGui::CalcTextSize(titleLightingSettings).x) / 2);
        ImGui::TextColored(ImVec4(0, 1, 1, 0.7), titleLightingSettings);
        ImGui::Spacing();
        ImGui::SameLine(10, 0);
        // --------------------------------------------------------------------------------------------
        /*/ Освещение Сцены
        {
            bool lighting = psDeviceFlags.test(rsLighting);
            if (ImGui::Checkbox("Lighting", &lighting))
            {
                psDeviceFlags.set(rsLighting, lighting);
                UI->RedrawScene();
            }
            if (ImGui::IsItemHovered())
            {
                ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
                // ImGui::SetTooltip("Turn off/on the general light of the entire scene. Turning off this light may be necessary\n to use the settings below to make the scene lighting closer to the game view."_RU >> u8"Выкл/вкл общего света всей сцены. Выключение этого света может потребоваться,\n чтоб с помощью настроек ниже сделать освещение сцены приближённое к игровому виду.");
            }
        }
        // --------------------------------------------------------------------------------------------*/
        ImGui::Spacing();
        ImGui::Separator();
        ImGui::Spacing();
        ImGui::SameLine(10, 0);
        // --------------------------------------------------------------------------------------------
        // Отображение солнца
        {
            if (ImGui::Checkbox("Sun Visible", &m_sun_visible))
            {
                IsShowSunChanged();
                UI->RedrawScene();
            }
            if (ImGui::IsItemHovered())
            {
                ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
                ImGui::SetTooltip("Turn Off/on showing the sun.");
            }
        }
        // --------------------------------------------------------------------------------------------
        ImGui::Spacing();
        ImGui::SameLine(10, 0);
        // --------------------------------------------------------------------------------------------
        // Использовать погодное движение солнца
        {
            if (ImGui::Checkbox("Use Sun Dir", &m_use_sun_dir))
            {
                IsUseSunDirChanged();
                UI->RedrawScene();
            }
            if (ImGui::IsItemHovered())
            {
                ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
                // ImGui::SetTooltip("Turn Off/on using the weather movement of the sun.\n If enabled, solar illumination will correspond to the position of the sun in weather cycles,\n otherwise the sun will be static and correspond to the position set in the global settings."_RU >>
                // u8"Выкл/вкл использование погодного движения солнца.\n Если включить, то освещение солнцем будет сооответствовать положению солнца в погодных циклах,\n в ином случае солнце будет статично и соответствовать положению выставленному в глобальных настройках.");
            }
        }
        // --------------------------------------------------------------------------------------------
        ImGui::Spacing();
        ImGui::SameLine(10, 0);
        // --------------------------------------------------------------------------------------------
        // Использовать Hemi
        {
            if (ImGui::Checkbox("Use Hemi", &m_use_hemi))
            {
                IsUseHemiChanged();
                UI->RedrawScene();
            }
            if (ImGui::IsItemHovered())
            {
                ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
                ImGui::SetTooltip("Turn Off/on using Hemi lighting.");
            }
        }
    }
    // --------------------------------------------------------------------------------------------
    ImGui::Spacing();
    ImGui::Separator();
    ImGui::Spacing();
    // --------------------------------------------------------------------------------------------
    // Прочие настройки
    pcstr titleOther = "Other settings";
    ImGui::SetCursorPosX((ImGui::GetWindowWidth() - ImGui::CalcTextSize(titleOther).x) / 2);
    ImGui::TextColored(ImVec4(0, 1, 1, 0.7), titleOther);
    // --------------------------------------------------------------------------------------------
    ImGui::Spacing();
    ImGui::SameLine(10, 0);
    // --------------------------------------------------------------------------------------------
    /*/ Коллизия дождя
    {
        if (ImGui::Checkbox("Collision of raindrops", &m_raindrop_collision))
        {
            IsRaindropCollisionChanged();
            UI->RedrawScene();
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Turn On/Off Collision of raindrops.");
        }
    }
    // --------------------------------------------------------------------------------------------
    ImGui::Spacing();
    ImGui::SameLine(10, 0);
    // --------------------------------------------------------------------------------------------
    // Звук дождя по крыше
    {
        if (ImGui::Checkbox("Sound of rain on roof", &m_snd_on_roof))
        {
            IsSndOnRoofChanged();
            g_pGamePersistent->Environment().ED_Reload();
            UI->RedrawScene();
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Turn On/Off sound of rain on roof.");
        }
    }*/
    // --------------------------------------------------------------------------------------------
    ImGui::Spacing();
    ImGui::SameLine(10, 0);
    // --------------------------------------------------------------------------------------------
    // Туман
    {
        bool UseFog = psDeviceFlags.test(rsFog);
        if (ImGui::Checkbox("Fog", &UseFog))
        {
            psDeviceFlags.set(rsFog, UseFog);
            UI->RedrawScene();
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Turn On/Off fog in the scene.");
        }
    }
    // --------------------------------------------------------------------------------------------
    ImGui::Spacing();
    ImGui::SameLine(10, 0);
    // --------------------------------------------------------------------------------------------
    // В реальном времени
    {
        bool RealTime = psDeviceFlags.test(rsRenderRealTime);
        if (ImGui::Checkbox("Real Time", &RealTime))
        {
            psDeviceFlags.set(rsRenderRealTime, RealTime);
        }
        if (ImGui::IsItemHovered())
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
    }
    // --------------------------------------------------------------------------------------------
    ImGui::Spacing();
    ImGui::SameLine(10, 0);
    // --------------------------------------------------------------------------------------------
    // Отключить звуки
    {
        bool MuteSounds = psDeviceFlags.test(rsMuteSounds);
        if (ImGui::Checkbox("Mute Sounds", &MuteSounds))
        {
            psDeviceFlags.set(rsMuteSounds, MuteSounds);
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Disable all sounds in the editor.");
        }
    }
    // --------------------------------------------------------------------------------------------
    ImGui::Spacing();
    ImGui::SameLine(10, 0);
    // --------------------------------------------------------------------------------------------
    // Статистика
    {
        bool sStats = psDeviceFlags.test(rsStatistic);
        if (ImGui::Checkbox("Stats", &sStats))
        {
            psDeviceFlags.set(rsStatistic, sStats);
            UI->RedrawScene();
        }
        if (ImGui::IsItemHovered())
        {
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
            ImGui::SetTooltip("Enable statistics display.");
        }
    }
    // --------------------------------------------------------------------------------------------
    ImGui::Spacing();
    ImGui::SameLine(10, 0);
    // --------------------------------------------------------------------------------------------
    // Показать Сетку
    {
        bool DrawGrid = psDeviceFlags.test(rsDrawGrid);
        if (ImGui::Checkbox("Draw Grid", &DrawGrid))
        {
            psDeviceFlags.set(rsDrawGrid, DrawGrid);
            UI->RedrawScene();
        }
        if (ImGui::IsItemHovered())
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
    }
    // --------------------------------------------------------------------------------------------
    ImGui::Spacing();
    ImGui::EndChild();
    ImGui::Spacing();
    // --------------------------------------------------------------------------------------------
    // Кнопка "Закрыть"
    {
        ImGui::SetCursorPosX((ImGui::GetWindowWidth() - sizeButton.x - (sizeButton.x + 3)) / 2);
        if (ImGui::Button("Apply", sizeButton))
        {
            if (selectedWeather)
                env.SetWeather(EPrefs->sWeather, true);
            env.fTimeFactor = m_speed_time;
            UI->RedrawScene();
        }
        if (ImGui::IsItemHovered())
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
    }
    // --------------------------------------------------------------------------------------------
    ImGui::SameLine();
    // --------------------------------------------------------------------------------------------
    // Кнопка "Закрыть"
    {
        ImGui::SetCursorPosX((ImGui::GetWindowWidth() - sizeButton.x + (sizeButton.x + 3)) / 2);
        if (ImGui::Button("Close", sizeButton))
        {
            UI->RedrawScene();
            bOpen = false;
        }
        if (ImGui::IsItemHovered())
            ImGui::SetMouseCursor(ImGuiMouseCursor_Hand);
    }
    // --------------------------------------------------------------------------------------------
    ImGui::Spacing();
}

void UIWeatherPropForm::Update()
{
    if (!Form)
        return;

    if (!Form->IsClosed())
    {
        ImGui::SetNextWindowPos(ImGui::GetWindowViewport()->GetCenter(), ImGuiCond_Appearing, { 0.5f, 0.5f });
        if (ImGui::Begin("Weather Properties", nullptr, ImGuiWindowFlags_NoDocking | ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoResize/* | ImGuiWindowFlags_NoBackground*/))
        {
            Form->Draw();
            ImGui::End();
        }
    }
    else
    {
        xr_delete(Form);
    }
}

void UIWeatherPropForm::Show()
{
    if (!Form)
        Form = new UIWeatherPropForm();
}

void UIWeatherPropForm::IsShowSunChanged() const
{
    if (m_sun_visible && !bIsShowSun)
        bIsShowSun = true;
    else
        bIsShowSun = false;
}

void UIWeatherPropForm::IsUseSunDirChanged() const
{
    if (m_use_sun_dir && !bIsUseSunDir)
        bIsUseSunDir = true;
    else
        bIsUseSunDir = false;
}

void UIWeatherPropForm::IsUseHemiChanged() const
{
    if (m_use_hemi && !bIsUseHemi)
        bIsUseHemi = true;
    else
        bIsUseHemi = false;
}
/*
void UIWeatherPropForm::IsRaindropCollisionChanged() const
{
    if (m_raindrop_collision && !bIsRaindropCollision)
        bIsRaindropCollision = true;
    else
        bIsRaindropCollision = false;
}

void UIWeatherPropForm::IsSndOnRoofChanged() const
{
    if (m_snd_on_roof && !bIsSndOnRoof)
        bIsSndOnRoof = true;
    else
        bIsSndOnRoof = false;
}*/
