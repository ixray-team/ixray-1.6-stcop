#include "stdafx.h"
#include "pch_script.h"
#include "UIKnifeParams.h"
#include "../../xrUI/UIXmlInit.h"
#include "../level.h"
#include "game_base_space.h"
#include "../ai_space.h"
#include "../../xrScripts/script_engine.h"
#include "inventory_item_object.h"
#include "UIInventoryUtilities.h"
#include "WeaponKnife.h"
#include "../xrEngine/string_table.h"

u32 const red_clr = color_argb(255, 210, 50, 50);
u32 const green_clr = color_argb(255, 50, 255, 50);

struct SLuaKnifeParams
{
	luabind::functor<float>		m_functorDamage;
	luabind::functor<float>		m_functorHandling;

	SLuaKnifeParams();
	~SLuaKnifeParams();
};

SLuaKnifeParams::SLuaKnifeParams()
{
	bool	functor_exists;
	functor_exists	= ai().script_engine().functor("ui_wpn_params.GetDamage",	m_functorDamage);	VERIFY(functor_exists);
	functor_exists	= ai().script_engine().functor("ui_wpn_params.GetHandling", m_functorHandling);	VERIFY(functor_exists);
}

SLuaKnifeParams::~SLuaKnifeParams()
{
}

SLuaKnifeParams* g_lua_knife_params = nullptr;

void destroy_lua_knife_params()
{
	if(g_lua_knife_params)
		xr_delete(g_lua_knife_params);
}

// =====================================================================

CUIKnifeParams::CUIKnifeParams()
{
	AttachChild(&m_Prop_line);

	AttachChild(&m_icon_dam);
	AttachChild(&m_icon_han);
	AttachChild(&m_icon_dist);

	AttachChild(&m_textDamage);
	AttachChild(&m_textHandling);
	AttachChild(&m_textDist);

	AttachChild(&m_progressDamage);
	AttachChild(&m_progressHandling);
	AttachChild(&m_textDist1Value);
	AttachChild(&m_textDist2Value);
	AttachChild(&m_meters_name);
	AttachChild(&m_textDistDelimiter);
}

CUIKnifeParams::~CUIKnifeParams()
{
}

void CUIKnifeParams::InitFromXml(CUIXml& xml_doc)
{
	if (!xml_doc.NavigateToNode("knife_params", 0))	return;
	CUIXmlInit::InitWindow			(xml_doc, "knife_params", 0, this);
	CUIXmlInit::InitStatic			(xml_doc, "knife_params:prop_line",			  0, &m_Prop_line);

	CUIXmlInit::InitStatic			(xml_doc, "knife_params:static_damage",		  0, &m_icon_dam);
	CUIXmlInit::InitStatic			(xml_doc, "knife_params:static_handling",	  0, &m_icon_han);

	CUIXmlInit::InitTextWnd			(xml_doc, "knife_params:cap_damage",		  0, &m_textDamage);
	CUIXmlInit::InitTextWnd			(xml_doc, "knife_params:cap_handling",		  0, &m_textHandling);

	CUIXmlInit::InitStatic			(xml_doc, "knife_params:static_dist",		  0, &m_icon_dist);
	CUIXmlInit::InitTextWnd			(xml_doc, "knife_params:cap_dist",			  0, &m_textDist);
	CUIXmlInit::InitTextWnd			(xml_doc, "knife_params:value_dist1",		  0, &m_textDist1Value);
	CUIXmlInit::InitTextWnd			(xml_doc, "knife_params:value_dist2",		  0, &m_textDist2Value);
	CUIXmlInit::InitTextWnd			(xml_doc, "knife_params:value_dist_delimiter",0, &m_textDistDelimiter);
	CUIXmlInit::InitTextWnd			(xml_doc, "knife_params:meters_name",		  0, &m_meters_name);

	m_progressDamage.InitFromXml	( xml_doc, "knife_params:progress_damage" );
	m_progressHandling.InitFromXml	( xml_doc, "knife_params:progress_handling" );
}

void CUIKnifeParams::SetInfo(CInventoryItem* slot_wpn, CInventoryItem& cur_wpn)
{
	if (!g_lua_knife_params)
	{
		g_lua_knife_params = new SLuaKnifeParams();
	}

	LPCSTR cur_section = cur_wpn.object().cNameSect().c_str();
	string2048 str_upgrades;
	str_upgrades[0] = 0;
	cur_wpn.get_upgrades_str(str_upgrades);

	float cur_hand = iFloor(g_lua_knife_params->m_functorHandling(cur_section, str_upgrades) * 53.0f) / 53.0f;
	float cur_damage = iFloor(g_lua_knife_params->m_functorDamage(cur_section, str_upgrades) * 53.0f) / 53.0f;

	float slot_hand = cur_hand;
	float slot_damage = cur_damage;

	if (slot_wpn && (slot_wpn != &cur_wpn))
	{
		LPCSTR slot_section = slot_wpn->object().cNameSect().c_str();
		str_upgrades[0] = 0;
		slot_wpn->get_upgrades_str(str_upgrades);

		slot_hand = iFloor(g_lua_knife_params->m_functorHandling(slot_section, str_upgrades) * 53.0f) / 53.0f;
		slot_damage = iFloor(g_lua_knife_params->m_functorDamage(slot_section, str_upgrades) * 53.0f) / 53.0f;
	}

	m_progressDamage.SetTwoPos(cur_damage, slot_damage);
	m_progressHandling.SetTwoPos(cur_hand, slot_hand);

	CWeaponKnife* knife = smart_cast<CWeaponKnife*>(&cur_wpn);
	float dist1 = knife->GetHit1Dist();
	float dist2 = knife->GetHit2Dist();
	float dist1_sl = dist1;
	float dist2_sl = dist2;
	
	if (slot_wpn)
	{
		CWeaponKnife* slot_knife = smart_cast<CWeaponKnife*>(slot_wpn);
		if (slot_knife)
		{
			dist1_sl = slot_knife->GetHit1Dist();
			dist2_sl = slot_knife->GetHit2Dist();
		}
	}
	
	if (dist1 == dist1_sl)
		m_textDist1Value.SetTextColor(m_meters_name.GetTextColor());
	else if (dist1 < dist1_sl)
		m_textDist1Value.SetTextColor(red_clr);
	else
		m_textDist1Value.SetTextColor(green_clr);

	if (dist2 == dist2_sl)
		m_textDist2Value.SetTextColor(m_meters_name.GetTextColor());
	else if (dist2 < dist2_sl)
		m_textDist2Value.SetTextColor(red_clr);
	else
		m_textDist2Value.SetTextColor(green_clr);

	string128 str;
	xr_sprintf(str, sizeof(str), "%.1f", dist1);
	m_textDist1Value.SetText(str);
	//m_textDist2Value.SetTextColor(color_rgba(220, 255, 220, 255));

	xr_sprintf(str, sizeof(str), "%.1f", dist2);
	m_textDist2Value.SetText(str);
	xr_sprintf(str, sizeof(str), "%s", *g_pStringTable->translate("st_m"));
	m_meters_name.SetText(str);
	xr_sprintf(str, sizeof(str), "|");
	m_textDistDelimiter.SetText(str);
}

bool CUIKnifeParams::Check(CInventoryItem& cur_wpn)
{
	CWeaponKnife* knife = smart_cast<CWeaponKnife*>(&cur_wpn);
	return knife && knife->m_bShowKnifeStats;
}