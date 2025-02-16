#include "StdAfx.h"
#include "UIActorProtectionInfo.h"
#include "UIXmlInit.h"
#include "UIStatic.h"
#include "UIScrollView.h"
#include "../actor.h"
#include "../actorcondition.h"
#include "../CustomOutfit.h"
#include "../string_table.h"
#include "../CHelmet.h"

UIActorProtectionInfo::UIActorProtectionInfo()
{
	Memory.mem_fill			(m_items, 0, sizeof(m_items));
}

UIActorProtectionInfo::~UIActorProtectionInfo()
{
	for(u32 i=_item_start; i<_max_item_index; ++i)
	{
		CUIStatic* _s			= m_items[i];
		xr_delete				(_s);
	}
}

struct LineDesc {
	LPCSTR staticName;
	LPCSTR paramName;
};

LineDesc infoLines[] = {
	{ "burn_immunity",					"ui_inv_outfit_burn_protection",			},
	{ "strike_immunity",				"ui_inv_outfit_strike_protection",			},
	{ "shock_immunity",					"ui_inv_outfit_shock_protection",			},
	{ "wound_immunity",					"ui_inv_outfit_wound_protection",			},
	{ "radiation_immunity",				"ui_inv_outfit_radiation_protection",		},
	{ "telepatic_immunity",				"ui_inv_outfit_telepatic_protection",		},
	{ "chemical_burn_immunity",			"ui_inv_outfit_chemical_burn_protection",	},
	{ "explosion_immunity",				"ui_inv_outfit_explosion_protection",		},
	{ "fire_wound_immunity",			"ui_inv_outfit_fire_wound_protection",		},
	{ "fire_wound_immunity_head",		"ui_inv_outfit_fire_wound_protection_head", }
};

void UIActorProtectionInfo::InitFromXml(CUIXml& xml_doc)
{
	LPCSTR base				= "outfit_info";

	string256					buff;
	CUIXmlInit::InitWindow		(xml_doc, base, 0, this);
	m_showLabels				= (xml_doc.ReadAttribInt(base, 0, "show_labels", 1) == 1);
	m_showHints					= (xml_doc.ReadAttribInt(base, 0, "show_hints", 1) == 1);
	m_showHeadArmor				= (xml_doc.ReadAttribInt(base, 0, "show_head_armor", 0) == 1);

	m_listWnd					= new CUIScrollView(); m_listWnd->SetAutoDelete(true);
	AttachChild					(m_listWnd);
	xr_strconcat				(buff, base, ":scroll_view");
	CUIXmlInit::InitScrollView	(xml_doc, buff, 0, m_listWnd);

	for (u32 i = _item_start; i < _max_item_index; ++i)
	{
		m_items[i]				= new CUIStatic();
		CUIStatic* st			= m_items[i];
		st->SetAutoDelete		(false);
		xr_strconcat			(buff, base, ":static_", infoLines[i].staticName);
		CUIXmlInit::InitStatic	(xml_doc, buff,	0, st);
	}

}

void UIActorProtectionInfo::Update(CCustomOutfit* outfit, CHelmet* helmet)
{
	m_outfit				= outfit;
	m_helmet				= helmet;

	m_listWnd->Clear();
    SetItem(_item_burn_immunity,			ALife::eHitTypeBurn);
	SetItem(_item_shock_immunity,			ALife::eHitTypeShock);
	SetItem(_item_strike_immunity,			ALife::eHitTypeStrike);
	SetItem(_item_wound_immunity,			ALife::eHitTypeWound);
	SetItem(_item_radiation_immunity,		ALife::eHitTypeRadiation);
	SetItem(_item_telepatic_immunity,		ALife::eHitTypeTelepatic);
    SetItem(_item_chemical_burn_immunity,	ALife::eHitTypeChemicalBurn);
	SetItem(_item_explosion_immunity,		ALife::eHitTypeExplosion);

	SetItemArmor(_item_armor_body, &COutfitBase::GetArmorBody, ALife::eHitTypeFireWound);
	if (m_showHeadArmor)
	{
		SetItemArmor(_item_armor_head, &COutfitBase::GetArmorHead);
	}
}

static void AppendValue(char (&dst)[128], float val, LPCSTR posColor = "%c[green]", LPCSTR negColor = "%c[red]")
{
	if (!fis_zero(val))
	{
		string128 buf;
		xr_sprintf(buf, "%s %+3.0f", (val > 0.0f) ? posColor : negColor, val*100.0f);
		xr_strcat(dst, buf);
	}
}

void UIActorProtectionInfo::SetItem(InfoType t, u32 hitType, bool force_add)
{
	string128 buf = "";

	float valOutfit		= m_outfit ? m_outfit->GetDefHitTypeProtection(ALife::EHitType(hitType)) : 0.0f;
	float valHelmet		= m_helmet ? m_helmet->GetDefHitTypeProtection(ALife::EHitType(hitType)) : 0.0f;
	float valAf			= 1.0f - Actor()->HitArtefactsOnBelt(1.0f,ALife::EHitType(hitType));
	float valBooster	= 1.0f - Actor()->HitBoosters(1.0f,ALife::EHitType(hitType));

	if (fsimilar(valOutfit, 0.0f) && fsimilar(valAf, 0.0f) && fsimilar(valBooster, 0.0f) && fsimilar(valHelmet, 0.0f) && !force_add)
	{
		return;
	}

	AppendValue(buf, valOutfit);
	AppendValue(buf, valHelmet);
	AppendValue(buf, valAf);
	AppendValue(buf, valBooster, "%c[ui_pda_orange]", "%c[ui_pda_violet]");

	SetItem(t, buf, force_add);
}

void UIActorProtectionInfo::SetItemArmor(InfoType t, float (COutfitBase::*getter)(), u32 hitType)
{
	float valOutfit		= m_outfit ? (m_outfit->*getter)() : 0.0f;
	float valHelmet		= m_helmet ? (m_helmet->*getter)() : 0.0f;
	float valAf			= 0.f;
	float valBooster	= 0.f;
	if (hitType != (u32)-1)
	{
		valAf		= 1.0f - Actor()->HitArtefactsOnBelt(1.0f,ALife::EHitType(hitType));
		valBooster	= 1.0f - Actor()->HitBoosters(1.0f,ALife::EHitType(hitType));
	}

	if (valOutfit < EPS_S && valHelmet < EPS_S && fis_zero(valAf) && fis_zero(valBooster))
	{
		return;
	}

	string128 buf = "";
	if (valOutfit > EPS_S)
	{
		AppendValue(buf, valOutfit);
	}
	if (valHelmet > EPS_S)
	{
		AppendValue(buf, valHelmet);
	}
	AppendValue(buf, valAf);
	AppendValue(buf, valBooster, "%c[ui_pda_orange]", "%c[ui_pda_violet]");
	SetItem(t, buf);
}

void UIActorProtectionInfo::SetItem(InfoType t, LPCSTR text, bool force_add)
{
	CUIStatic* st = m_items[t];

	if ((text == nullptr || xr_strlen(text) == 0) && !force_add)
	{
		return;
	}
	
	LPCSTR paramName	= *CStringTable().translate(infoLines[t].paramName);
	if (m_showLabels)
	{
		string128 buff;
		xr_sprintf(buff, sizeof(buff), "%s %s", paramName, text);
		st->TextItemControl()->SetText(buff);
	}
	else
	{
		st->TextItemControl()->SetText(text);
	}
	if (m_showHints)
	{
		st->m_stat_hint_text = paramName;
	}

	if (st->GetParent() == nullptr)
		m_listWnd->AddWindow(st, false);
}
