#include "stdafx.h"
#include "UIMainIngameWnd.h"
#include "UICarPanel.h"
#include "UIXmlInit.h"
#include "../hudmanager.h"

const LPCSTR POINTER_ARROW_TEX = "ui\\ui_car_arrow";

void CUICarPanel::Init			(float x, float y, float width, float height)
{
	CUIXml uiXml;

	string128		CAR_PANEL_XML;
	xr_sprintf		(CAR_PANEL_XML, "car_panel_%d.xml", ui_hud_type);

	uiXml.Load(CONFIG_PATH, UI_PATH, CAR_PANEL_XML);

	CUIXmlInit	xml_init;
	////////////////////////////////////////////////////////////////////
	AttachChild(&UIStaticCarHealth);
	xml_init.InitStatic(uiXml, "car_health_static", 0, &UIStaticCarHealth);

	UIStaticCarHealth.AttachChild(&UICarHealthBar);
	xml_init.InitAutoStaticGroup(uiXml,"car_health_static", 0, &UIStaticCarHealth);
	xml_init.InitProgressBar(uiXml, "car_health_progress_bar", 0, &UICarHealthBar);

	AttachChild(&UIStaticCarFuel);
	xml_init.InitStatic(uiXml, "car_fuel_static", 0, &UIStaticCarFuel);

	UIStaticCarFuel.AttachChild(&UICarFuelBar);
	xml_init.InitAutoStaticGroup(uiXml,"car_fuel_static", 0, &UIStaticCarFuel);
	xml_init.InitProgressBar(uiXml, "car_fuel_progress_bar", 0, &UICarFuelBar);

	AttachChild(&UISpeedometer);
	xml_init.InitStatic(uiXml, "speedometer", 0, &UISpeedometer);
	string256 buf;
	strconcat(sizeof(buf),buf,"speedometer", ":pointer");
	float offset_x = uiXml.ReadAttribFlt(buf, 0, "offset_x", 0);
	float offset_y = uiXml.ReadAttribFlt(buf, 0, "offset_y", 0);
	float angle_min = uiXml.ReadAttribFlt(buf, 0, "angle_min", -60);
	float angle_max = uiXml.ReadAttribFlt(buf, 0, "angle_max", 60);
	LPCSTR pointer_tex = uiXml.ReadAttrib(buf, 0, "arrow", POINTER_ARROW_TEX);
	UISpeedometer.InitPointer(pointer_tex, offset_x, offset_y, -angle_min * M_PI / 180.0f, -angle_max * M_PI / 180.0f);
	SetSpeed(0.3f);

	AttachChild(&UITachometer);
	xml_init.InitStatic(uiXml, "tachometer", 0, &UITachometer);
	strconcat(sizeof(buf),buf,"tachometer", ":pointer");
	offset_x = uiXml.ReadAttribFlt(buf, 0, "offset_x", 0);
	offset_y = uiXml.ReadAttribFlt(buf, 0, "offset_y", 0);
	angle_min = uiXml.ReadAttribFlt(buf, 0, "angle_min", -60);
	angle_max = uiXml.ReadAttribFlt(buf, 0, "angle_max", 60);
	pointer_tex = uiXml.ReadAttrib(buf, 0, "arrow", POINTER_ARROW_TEX);
	UITachometer.InitPointer(pointer_tex, offset_x, offset_y, -angle_min * M_PI / 180.0f, -angle_max * M_PI / 180.0f);
	SetSpeed(0.3f);

	Show(false);
	Enable(false);

	inherited::SetWndRect(x, y, width, height);
}

//////////////////////////////////////////////////////////////////////////

void CUICarPanel::SetCarHealth(float value)
{
	float pos = value*100;
	clamp(pos, 0.0f, 100.0f);
	UICarHealthBar.SetProgressPos(pos);
}

//////////////////////////////////////////////////////////////////////////

void CUICarPanel::SetSpeed(float speed)
{
	clamp(speed,0.f,1.f);
	UISpeedometer.SetValue(speed);
}

//////////////////////////////////////////////////////////////////////////

void CUICarPanel::SetRPM(float rpm)
{
	clamp(rpm,0.f,1.f);
	UITachometer.SetValue(rpm);
}

//////////////////////////////////////////////////////////////////////////

void CUICarPanel::SetFuel(float fuel)
{
	clamp(fuel,0.f,100.0f);
	UICarFuelBar.SetProgressPos(fuel);
}