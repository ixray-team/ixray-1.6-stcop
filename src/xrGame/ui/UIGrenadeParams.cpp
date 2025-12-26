////////////////////////////////////////////////////////////////////////////
//	Module 		: UIGrenadeParams.cpp
//	Created 	: 03.08.2025
//	Author		: St4lker0k765
//	Description : Implementation for grenade params in inventory
////////////////////////////////////////////////////////////////////////////
#include "stdafx.h"
#include "pch_script.h"
#include "UIGrenadeParams.h"
#include "../../xrUI/UIXmlInit.h"
#include "../Level.h"
#include "game_base_space.h"
#include "../ai_space.h"
#include "../../xrScripts/script_engine.h"
#include "inventory_item_object.h"
#include "UIInventoryUtilities.h"
#include "Grenade.h"
#include "../xrEngine/string_table.h"
#include "../../xrUI/UIHelper.h"

CUIGrenadeParams::CUIGrenadeParams()
{}

CUIGrenadeParams::~CUIGrenadeParams()
{}

void CUIGrenadeParams::InitFromXml(CUIXml& xml_doc)
{
	if (!xml_doc.NavigateToNode("grenade_params", 0))	
		return;

	CUIXmlInit::InitWindow			(xml_doc, "grenade_params", 0, this);

	m_Prop_line						= UIHelper::CreateStatic(xml_doc, "grenade_params:prop_line", this);

	m_iconBlastHit					= UIHelper::CreateStatic(xml_doc, "grenade_params:static_blast_hit", this);
	m_iconBlastRadius				= UIHelper::CreateStatic(xml_doc, "grenade_params:static_blast_radius", this);
	m_iconFragsCount				= UIHelper::CreateStatic(xml_doc, "grenade_params:static_frags_count", this);
	m_iconFragsRadius				= UIHelper::CreateStatic(xml_doc, "grenade_params:static_frags_radius", this);
	m_iconFragsHit					= UIHelper::CreateStatic(xml_doc, "grenade_params:static_frags_hit", this);

	m_captionBlastHit				= UIHelper::CreateStatic(xml_doc, "grenade_params:cap_blast_hit", this);
	m_captionBlastRadius			= UIHelper::CreateStatic(xml_doc, "grenade_params:cap_blast_radius", this);
	m_captionFragsCount				= UIHelper::CreateStatic(xml_doc, "grenade_params:cap_frags_count", this);
	m_captionFragsRadius			= UIHelper::CreateStatic(xml_doc, "grenade_params:cap_frags_radius", this);
	m_captionFragsHit				= UIHelper::CreateStatic(xml_doc, "grenade_params:cap_frags_hit", this);
	
	m_textBlastHit					= UIHelper::CreateStatic(xml_doc, "grenade_params:value_blast_hit", this);
	m_textBlastRadius				= UIHelper::CreateStatic(xml_doc, "grenade_params:value_blast_radius", this);
	m_textFragsCount				= UIHelper::CreateStatic(xml_doc, "grenade_params:value_frags_count", this);
	m_textFragsRadius				= UIHelper::CreateStatic(xml_doc, "grenade_params:value_frags_radius", this);
	m_textFragsHit					= UIHelper::CreateStatic(xml_doc, "grenade_params:value_frags_hit", this);
	initialized = true;
}

void CUIGrenadeParams::SetInfo(CInventoryItem* slot_wpn, CInventoryItem& cur_wpn)
{
	if (!initialized)
		return;

	LPCSTR cur_section = cur_wpn.object().cNameSect().c_str();
	string2048 str_upgrades;
	str_upgrades[0] = 0;
	cur_wpn.get_upgrades_str(str_upgrades);

	if (slot_wpn && (slot_wpn != &cur_wpn))
	{
		LPCSTR slot_section = slot_wpn->object().cNameSect().c_str();
		str_upgrades[0] = 0;
		slot_wpn->get_upgrades_str(str_upgrades);

	}

	const auto elementColorize = [&](CUIStatic* text, float first, float second)
	{
		constexpr u32 red_clr = color_argb(255, 210, 50, 50);
		constexpr u32 green_clr = color_argb(255, 50, 255, 50);

		if (first == second)
			text->SetTextColor(color_rgba(124, 119, 115, 255));
		else if (first < second)
			text->SetTextColor(red_clr);
		else
			text->SetTextColor(green_clr);
	};

	CGrenade* grenade = cur_wpn.cast_grenade();

	float blastHit = grenade->GetBlastHit();
	float blastHit_sl = blastHit;

	float blastRadius = grenade->GetBlastRadius();
	float blastRadius_sl = blastRadius;

	int fragsCount = grenade->GetFragsCount();
	int fragsCount_sl = fragsCount;

	float fragsRadius = grenade->GetFragsRadius();
	float fragsRadius_sl = fragsRadius;

	float fragsHit = grenade->GetFragsHit();
	float fragsHit_sl = fragsHit;

	if (slot_wpn)
	{
		CGrenade* slot_grenade = cur_wpn.cast_grenade();
		if (slot_grenade)
		{
			blastHit_sl = slot_grenade->GetBlastHit();
			blastRadius_sl = slot_grenade->GetBlastRadius();
			fragsCount_sl = slot_grenade->GetFragsCount();
			fragsRadius_sl = slot_grenade->GetFragsRadius();
			fragsHit_sl = slot_grenade->GetFragsHit();
		}
	}
	
	elementColorize(m_textBlastHit, blastHit, blastHit_sl);
	elementColorize(m_textBlastRadius, blastRadius, blastRadius_sl);
	elementColorize(m_textFragsCount, (float)fragsCount, (float)fragsCount_sl);
	elementColorize(m_textFragsRadius, fragsRadius, fragsRadius_sl);
	elementColorize(m_textFragsHit, fragsHit, fragsHit_sl);

	string128 str;
	xr_sprintf(str, sizeof(str), "%.1f", blastHit);
	m_textBlastHit->SetText(str);

	xr_sprintf(str, sizeof(str), "%.1f %s", blastRadius, g_pStringTable->translate("st_m").c_str());
	m_textBlastRadius->SetText(str);

	xr_sprintf(str, sizeof(str), "%d", fragsCount);
	m_textFragsCount->SetText(str);

	xr_sprintf(str, sizeof(str), "%.1f %s", fragsRadius, g_pStringTable->translate("st_m").c_str());
	m_textFragsRadius->SetText(str);

	xr_sprintf(str, sizeof(str), "%.1f", fragsHit);
	m_textFragsHit->SetText(str);
}

bool CUIGrenadeParams::Check(CInventoryItem& cur_wpn)
{
	CGrenade* grenade = cur_wpn.cast_grenade();
	return grenade;
}