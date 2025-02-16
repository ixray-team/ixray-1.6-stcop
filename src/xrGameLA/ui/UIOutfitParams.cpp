#include "stdafx.h"
#include "UIOutfitParams.h"
#include "UIStatic.h"
#include "UIXmlInit.h"
#include "../inventory_item.h"
#include "../CustomOutfit.h"
#include "../CHelmet.h"

CUIOutfitParams::CUIOutfitParams()
{
	Memory.mem_fill(m_info_items, 0, sizeof(m_info_items));
}

CUIOutfitParams::~CUIOutfitParams()
{
	for (u32 i = _item_start; i<_max_item_index; ++i)
	{
		CUIStatic* _s = m_info_items[i];
		xr_delete(_s);
	}
}

CUIOutfitParams::LineDesc CUIOutfitParams::infoLines[] = {
	{ "burn_immunity",					"ui_inv_outfit_burn_protection"				},
	{ "strike_immunity",				"ui_inv_outfit_strike_protection"			},
	{ "shock_immunity",					"ui_inv_outfit_shock_protection"			},
	{ "wound_immunity",					"ui_inv_outfit_wound_protection"			},
	{ "radiation_immunity",				"ui_inv_outfit_radiation_protection"		},
	{ "telepatic_immunity",				"ui_inv_outfit_telepatic_protection"		},
	{ "chemical_burn_immunity",			"ui_inv_outfit_chemical_burn_protection"	},
	{ "explosion_immunity",				"ui_inv_outfit_explosion_protection"		},
	{ "fire_wound_immunity",			"ui_inv_outfit_fire_wound_protection"		},
	{ "fire_wound_immunity_head",		"ui_inv_outfit_fire_wound_protection_head"	},
	{ "additional_inventory_weight2",	"ui_inv_outfit_add_weight"					},
	{ "sprint_allowed",					"ui_inv_outfit_sprint_allowed"				},

	{ "health_restore_speed",			"ui_inv_health"								},
	{ "radiation_restore_speed",		"ui_inv_radiation"							},
	{ "satiety_restore_speed",			"ui_inv_satiety"							},
	{ "power_restore_speed",			"ui_inv_power"								},
	{ "bleeding_restore_speed",			"ui_inv_bleeding"							},
};

CUIOutfitParams::ProtectionDesc CUIOutfitParams::protectionTypes[] = {
	{ALife::eHitTypeBurn,			CUIOutfitParams::_item_burn_immunity },
	{ALife::eHitTypeStrike,			CUIOutfitParams::_item_strike_immunity },
	{ALife::eHitTypeShock,			CUIOutfitParams::_item_shock_immunity },
	{ALife::eHitTypeWound,			CUIOutfitParams::_item_wound_immunity },
	{ALife::eHitTypeRadiation,		CUIOutfitParams::_item_radiation_immunity },
	{ALife::eHitTypeTelepatic,		CUIOutfitParams::_item_telepatic_immunity },
	{ALife::eHitTypeChemicalBurn,	CUIOutfitParams::_item_chemical_burn_immunity },
	{ALife::eHitTypeExplosion,		CUIOutfitParams::_item_explosion_immunity },
//	{ALife::eHitTypeFireWound,		CUIOutfitParams::_item_fire_wound_immunity },
};

void CUIOutfitParams::InitFromXml(CUIXml& xml_doc)
{
	LPCSTR base = "outfit_params";
	if (!xml_doc.NavigateToNode(base, 0))
	{
		base = "af_params";
	}
	if (!xml_doc.NavigateToNode(base, 0))	return;

	string256					buff;
	CUIXmlInit::InitWindow(xml_doc, base, 0, this);
	m_showHeadArmor = (xml_doc.ReadAttribInt(base, 0, "show_head_armor", 0) == 1);

	for (u32 i = _item_start; i<_max_item_index; ++i)
	{
		CUIStatic* st = m_info_items[i] = new CUIStatic();
		st->SetAutoDelete(false);
		xr_strconcat(buff, base, ":static_", infoLines[i].staticName);
		CUIXmlInit::InitStatic(xml_doc, buff, 0, st);
	}
}

bool CUIOutfitParams::Check(CInventoryItem& outfit)
{
	return !!pSettings->line_exist(*outfit.object().cNameSect(), "burn_protection");
}

#include "../../xrEngine/string_table.h"

void CUIOutfitParams::SetInfo(CInventoryItem& itm)
{
	auto outfitBase = smart_cast<COutfitBase*>(&itm);
	auto outfit		= smart_cast<CCustomOutfit*>(&itm);
	R_ASSERT2(outfitBase, "Item is not Outfit!");

	float h = 0.0f;
	DetachAll();

	int numProtections = sizeof(protectionTypes) / sizeof(protectionTypes[0]);
	for (u32 i = 0; i < numProtections; ++i)
	{
		DisplayProtecton(protectionTypes[i].infoType, protectionTypes[i].hitType, outfitBase, h);
	}
	DisplayArmor(_item_armor_body, outfitBase->GetArmorBody(), h);
	if (m_showHeadArmor)
	{
		DisplayArmor(_item_armor_head, outfitBase->GetArmorHead(), h);
	}
	if (outfit)
	{
		DisplayValue(_item_additional_inventory_weight2, outfit->m_additional_weight2, h);
	}
	DisplayValue(_item_sprint_allowed, itm.IsSprintAllowed(), h);

#pragma todo("Move these multipliers to XML, like in CoP")
	DisplayValue(_item_health_restore_speed,	outfitBase->GetHealthRestoreSpeed()		* 6600.f, h);
	DisplayValue(_item_radiation_restore_speed,	outfitBase->GetRadiationRestoreSpeed()	* 1000.f, h, true);
	DisplayValue(_item_satiety_restore_speed,	outfitBase->GetSatietyRestoreSpeed()	* 100000.f, h);
	DisplayValue(_item_power_restore_speed,		outfitBase->GetPowerRestoreSpeed()		* 2000.f, h);
	DisplayValue(_item_bleeding_restore_speed,	outfitBase->GetBleedingRestoreSpeed()	* 1000.f, h);

	SetHeight(h);
}

void CUIOutfitParams::DisplayProtecton(InfoType t, u32 hitType, COutfitBase* outfit, float &height)
{
	float value = outfit->GetDefHitTypeProtection((ALife::EHitType)hitType);

	DisplayValue(t, value * 100.f, height);
}

void CUIOutfitParams::DisplayValue(InfoType t, bool value, float& height)
{
	if (value == false)
	{
		string128 text;
		LPCSTR colorMark = "%c[red]";
		xr_sprintf(text, "%s%s", colorMark, CStringTable().translate(infoLines[t].paramName).c_str());

		DisplayValue(t, text, color_rgba(255, 125, 25, 255), height);
	}
}

void CUIOutfitParams::DisplayValue(InfoType t, float value, float& height, bool revertSign)
{
	if (!fis_zero(value))
	{
		string128 text;
		bool isGood = revertSign ? (value < 0) : (value > 0);
		LPCSTR colorMark = isGood ? "%c[green]" : "%c[red]";
		u32 texColor = isGood
			? color_rgba(25, 255, 25, 255)
			: color_rgba(255, 25, 25, 255);

		xr_sprintf(text, "%s %s %+.0f",
			CStringTable().translate(infoLines[t].paramName).c_str(),
			colorMark,
			value);

		DisplayValue(t, text, texColor, height);
	}
}

void CUIOutfitParams::DisplayValue(InfoType t, LPCSTR text, u32 color, float & height)
{
	if (text != nullptr)
	{
		CUIStatic* el = m_info_items[t];
		el->SetTextureColor(color);
		el->TextItemControl()->SetText(text);
		el->SetWndPos(el->GetWndPos().x, height);
		height += el->GetWndSize().y;
		AttachChild(el);
	}
}

void CUIOutfitParams::DisplayArmor(InfoType t, float armor, float& height)
{
	if (armor > EPS_S)
	{
		DisplayValue(t, armor * 100.f, height);
	}
}
