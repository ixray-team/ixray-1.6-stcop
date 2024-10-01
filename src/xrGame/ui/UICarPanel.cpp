#include "stdafx.h"
#include "UIMainIngameWnd.h"
#include "UICarPanel.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"

constexpr LPCSTR CAR_PANEL_XML = "car_panel.xml";
constexpr LPCSTR POINTER_ARROW_TEX = "ui\\hud_map_arrow";

void CUICarPanel::Init()
{
    CUIXml uiXml;
    uiXml.Load(CONFIG_PATH, UI_PATH, CAR_PANEL_XML);

    CUIXmlInit xml_init;
    xml_init.InitWindow(uiXml, "car_panel", 0, this);
    ////////////////////////////////////////////////////////////////////
    UIStaticCarHealth = UIHelper::CreateStatic(uiXml, "car_panel:car_static", this);
    UIStaticCarHealth->SetAutoDelete(true);

    UIEngineSpeedMode = UIHelper::CreateTextWnd(uiXml, "car_panel:car_speed_mode", UIStaticCarHealth);
    UIEngineSpeedMode->SetAutoDelete(true);

    UIEngineLamp = UIHelper::CreateStatic(uiXml, "car_panel:car_engine_lamp:on", UIStaticCarHealth);
    UIEngineLamp->SetAutoDelete(true);
    
    UIEngineLampOff = UIHelper::CreateStatic(uiXml, "car_panel:car_engine_lamp:off", UIStaticCarHealth);
    UIEngineLampOff->SetAutoDelete(true);

    UILightLamp = UIHelper::CreateStatic(uiXml, "car_panel:car_light_lamp:on", UIStaticCarHealth);
    UILightLamp->SetAutoDelete(true);
    
    UILightLampOff = UIHelper::CreateStatic(uiXml, "car_panel:car_light_lamp:off", UIStaticCarHealth);
    UILightLampOff->SetAutoDelete(true);

    UICarHealthBar = UIHelper::CreateProgressBar(uiXml, "car_panel:car_health_progress_bar", UIStaticCarHealth);
    UICarHealthBar->SetAutoDelete(true);
    
    UICarFuelBar = UIHelper::CreateProgressBar(uiXml, "car_panel:car_fuel_progress_bar", UIStaticCarHealth);
    UICarHealthBar->SetAutoDelete(true);

    Show(false);
}

void CUICarPanel::SetEngineLamp(bool On)
{
    if (On)
    {
        UIEngineLamp->Show(true);
        UIEngineLampOff->Show(false);
    }
    else
    {
        UIEngineLamp->Show(false);
        UIEngineLampOff->Show(true);
    }
}

void CUICarPanel::SetLightLamp(bool On)
{
    if (On)
    {
        UILightLamp->Show(true);
        UILightLampOff->Show(false);
    }
    else
    {
        UILightLamp->Show(false);
        UILightLampOff->Show(true);
    }
}

void CUICarPanel::Draw()
{
    inherited::Draw();
}

//////////////////////////////////////////////////////////////////////////

CUICarPanel::~CUICarPanel()
{
}

void CUICarPanel::SetCarHealth(float value)
{
    float pos = value;
    clamp(pos, 0.0f, 100.0f);
    UICarHealthBar->SetProgressPos(pos);
}

void CUICarPanel::SetCarFuel(float value)
{
    float pos = value;
    clamp(pos, 0.0f, 100.0f);
    UICarFuelBar->SetProgressPos(pos);
}

//////////////////////////////////////////////////////////////////////////

void CUICarPanel::SetSpeed(float speed)
{
    //	clamp(speed,0.f,1.f);
    //	UISpeedometer.SetValue(speed);
}

//////////////////////////////////////////////////////////////////////////

void CUICarPanel::SetRPM(float rpm)
{
    //	clamp(rpm,0.f,1.f);
    //	UITachometer.SetValue(rpm);
}

void CUICarPanel::SetSpeedMode(const xr_string& Mode)
{
    UIEngineSpeedMode->SetText(Mode.c_str());
}