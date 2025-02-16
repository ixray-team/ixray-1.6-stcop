#include "pch_script.h"
#include "UIWpnParams.h"
#include "UIXmlInit.h"
#include "../level.h"
#include "../game_base_space.h"
#include "../ai_space.h"
#include "../../xrScripts/script_engine.h"
#include "../inventory_item.h"
#include "../Silencer.h"
#include "../WeaponBinoculars.h"
#include "../WeaponMagazined.h"

struct SLuaWpnParams{
	luabind::functor<float>		m_functorRPM;
	luabind::functor<float>		m_functorAccuracy;
	luabind::functor<float>		m_functorDamage;
	luabind::functor<float>		m_functorDamageMP;
	luabind::functor<float>		m_functorHandling;
	SLuaWpnParams();
	~SLuaWpnParams();
};

SLuaWpnParams::SLuaWpnParams()
{
	bool	functor_exists;
	functor_exists	= ai().script_engine().functor("ui_wpn_params.GetRPM" ,		m_functorRPM);			VERIFY(functor_exists);
	functor_exists	= ai().script_engine().functor("ui_wpn_params.GetDamage" ,	m_functorDamage);		VERIFY(functor_exists);
	functor_exists	= ai().script_engine().functor("ui_wpn_params.GetDamageMP" ,m_functorDamageMP);		VERIFY(functor_exists);
	functor_exists	= ai().script_engine().functor("ui_wpn_params.GetHandling" ,m_functorHandling);		VERIFY(functor_exists);
	functor_exists	= ai().script_engine().functor("ui_wpn_params.GetAccuracy" ,m_functorAccuracy);		VERIFY(functor_exists);
}

SLuaWpnParams::~SLuaWpnParams()
{
}

SLuaWpnParams* g_lua_wpn_params = nullptr;

void destroy_lua_wpn_params()
{
	if(g_lua_wpn_params)
		xr_delete(g_lua_wpn_params);
}

CUIWpnParams::CUIWpnParams(){
	AttachChild(&m_textAccuracy);
	AttachChild(&m_textDamage);
	AttachChild(&m_textHandling);
	AttachChild(&m_textRPM);

	AttachChild(&m_progressAccuracy);
	AttachChild(&m_progressDamage);
	AttachChild(&m_progressHandling);
	AttachChild(&m_progressRPM);
}

CUIWpnParams::~CUIWpnParams()
{
}

void CUIWpnParams::InitFromXml(CUIXml& xml_doc){
	if (!xml_doc.NavigateToNode("wpn_params", 0))	return;
	CUIXmlInit::InitWindow			(xml_doc, "wpn_params", 0, this);

	CUIXmlInit::InitStatic			(xml_doc, "wpn_params:cap_accuracy",		0, &m_textAccuracy);
	CUIXmlInit::InitStatic			(xml_doc, "wpn_params:cap_damage",			0, &m_textDamage);
	CUIXmlInit::InitStatic			(xml_doc, "wpn_params:cap_handling",		0, &m_textHandling);
	CUIXmlInit::InitStatic			(xml_doc, "wpn_params:cap_rpm",				0, &m_textRPM);

	CUIXmlInit::InitProgressBar		(xml_doc, "wpn_params:progress_accuracy",	0, &m_progressAccuracy);
	CUIXmlInit::InitProgressBar		(xml_doc, "wpn_params:progress_damage",		0, &m_progressDamage);
	CUIXmlInit::InitProgressBar		(xml_doc, "wpn_params:progress_handling",	0, &m_progressHandling);
	CUIXmlInit::InitProgressBar		(xml_doc, "wpn_params:progress_rpm",		0, &m_progressRPM);

	m_progressAccuracy.SetRange		(0, 100);
	m_progressDamage.SetRange		(0, 100);
	m_progressHandling.SetRange		(0, 100);
	m_progressRPM.SetRange			(0, 100);

}

void CUIWpnParams::SetInfo(CInventoryItem& cur_wpn)
{

	if(!g_lua_wpn_params)
		g_lua_wpn_params = new SLuaWpnParams();

	LPCSTR cur_section = cur_wpn.object().cNameSect().c_str();
	string2048 str_upgrades = {0};
	cur_wpn.get_upgrades_str(str_upgrades);
	auto weapon = smart_cast<CWeaponMagazined*>(&cur_wpn);
	LPCSTR cur_ammo_section = weapon ? weapon->getAmmoName() : nullptr;

	float cur_rpm = iFloor(g_lua_wpn_params->m_functorRPM(cur_section, str_upgrades, cur_ammo_section)*53.0f) / 53.0f;
	float cur_accur = iFloor(g_lua_wpn_params->m_functorAccuracy(cur_section, str_upgrades, cur_ammo_section)*53.0f) / 53.0f;
	float cur_hand = iFloor(g_lua_wpn_params->m_functorHandling(cur_section, str_upgrades, cur_ammo_section)*53.0f) / 53.0f;
	float cur_damage = (GameID() == GAME_SINGLE) ?
		iFloor(g_lua_wpn_params->m_functorDamage(cur_section, str_upgrades, cur_ammo_section)*53.0f) / 53.0f
		: iFloor(g_lua_wpn_params->m_functorDamageMP(cur_section, str_upgrades, cur_ammo_section)*53.0f) / 53.0f;

	m_progressRPM.SetProgressPos(cur_rpm);
	m_progressAccuracy.SetProgressPos(cur_accur);
	m_progressDamage.SetProgressPos(cur_damage);
	m_progressHandling.SetProgressPos(cur_hand);
}

bool CUIWpnParams::Check(CInventoryItem& cur_wpn)
{
	LPCSTR wpn_section = cur_wpn.object().cNameSect().c_str();
	if (pSettings->line_exist(wpn_section, "fire_dispersion_base"))
	{
		if (smart_cast<CSilencer*>(&cur_wpn) != nullptr)
			return false;
		if (smart_cast<CWeaponBinoculars*>(&cur_wpn) != nullptr)
			return false;

		return true;
	}
	else
		return false;
}