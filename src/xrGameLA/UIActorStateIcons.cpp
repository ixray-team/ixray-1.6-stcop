#include "stdafx.h"
#include "UIActorStateIcons.h"
#include "UI/UIXmlInit.h"
#include "hudmanager.h"

CUIActorStateIcons::CUIActorStateIcons()
{
	
}

CUIActorStateIcons::~CUIActorStateIcons()
{
	
}

#define ATTACH_AND_INIT(icon_state, name)														\
	AttachChild										(&m_icons[icon_state]);						\
	xml_init.InitStatic								(xml, name, 0, &m_icons[icon_state]);		\
	m_icons[icon_state].Show						(false);									

void CUIActorStateIcons::Init()
{
	CUIXml		xml;

	string128		STATE_ICONS_XML;
	xr_sprintf		(STATE_ICONS_XML, "actor_state_icon_%d.xml", ui_hud_type);

	xml.Load			(CONFIG_PATH, UI_PATH, STATE_ICONS_XML);

	CUIXmlInit	xml_init;
	
	ATTACH_AND_INIT									(eJammedInactive,		"weapon_jamming_icon:inactive");
	ATTACH_AND_INIT									(eJammedRed,			"weapon_jamming_icon:red");
	ATTACH_AND_INIT									(eJammedYellow,		"weapon_jamming_icon:yellow");
	ATTACH_AND_INIT									(eJammedGreen,		"weapon_jamming_icon:green");

	ATTACH_AND_INIT									(eRadiationInactive,		"radiation_icon:inactive");
	ATTACH_AND_INIT									(eRadiationRed,			"radiation_icon:red");
	ATTACH_AND_INIT									(eRadiationYellow,		"radiation_icon:yellow");
	ATTACH_AND_INIT									(eRadiationGreen,		"radiation_icon:green");

	ATTACH_AND_INIT									(eBleedingInactive,		"bleeding_icon:inactive");
	ATTACH_AND_INIT									(eBleedingRed,			"bleeding_icon:red");
	ATTACH_AND_INIT									(eBleedingYellow,		"bleeding_icon:yellow");
	ATTACH_AND_INIT									(eBleedingGreen,		"bleeding_icon:green");

	ATTACH_AND_INIT									(eHungerInactive,		"hunger_icon:inactive");
	ATTACH_AND_INIT									(eHungerRed,		"hunger_icon:red");
	ATTACH_AND_INIT									(eHungerYellow,		"hunger_icon:yellow");
	ATTACH_AND_INIT									(eHungerGreen,		"hunger_icon:green");

	ATTACH_AND_INIT									(eThirstInactive,		"thirst_icon:inactive");
	ATTACH_AND_INIT									(eThirstRed,		"thirst_icon:red");
	ATTACH_AND_INIT									(eThirstYellow,		"thirst_icon:yellow");
	ATTACH_AND_INIT									(eThirstGreen,		"thirst_icon:green");

	ATTACH_AND_INIT									(ePsyHealthInactive,		"psi_damage_icon:inactive");
	ATTACH_AND_INIT									(ePsyHealthRed,			"psi_damage_icon:red");
	ATTACH_AND_INIT									(ePsyHealthYellow,		"psi_damage_icon:yellow");
	ATTACH_AND_INIT									(ePsyHealthGreen,		"psi_damage_icon:green");

	ATTACH_AND_INIT									(eSleepInactive,		"sleep_icon:inactive");
	ATTACH_AND_INIT									(eSleepRed,			"sleep_icon:red");
	ATTACH_AND_INIT									(eSleepYellow,		"sleep_icon:yellow");
	ATTACH_AND_INIT									(eSleepGreen,		"sleep_icon:green");
}

void CUIActorStateIcons::SetIcons(Flags32 set)
{
	for (u16 i = 0; i < MAX_ICONS; i++)
	{
		m_icons[i].Show								(set.test(1 << i) ? true : false);
	}
}