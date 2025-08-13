#include "StdAfx.h"
#include "ui_af_params.h"
#include "../../xrUI/Widgets/UIStatic.h"

#include "../Actor.h"
#include "../Artefact.h"
#include "../ActorCondition.h"
#include "../inventory_item.h"

#include "object_broker.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrEngine/string_table.h"

CUIArtefactParams::CUIArtefactParams(const CParamType& type)
{
	for ( u32 i = 0; i < ALife::eHitTypeWound_2; ++i )
	{
		m_immunity_item[i] = nullptr;
	}
	for ( u32 i = 0; i < ALife::eRestoreTypeMax; ++i )
	{
		m_restore_item[i] = nullptr;
	}

	m_disp_condition = nullptr;
	m_additional_weight = nullptr;
	m_af_slots = nullptr;

	object_type = type;
	m_Prop_line = nullptr;
}

CUIArtefactParams::~CUIArtefactParams()
{
	delete_data	( m_immunity_item );
	delete_data	( m_restore_item );
	xr_delete(m_disp_condition);
	xr_delete	( m_additional_weight );
	xr_delete	(m_af_slots);
	xr_delete	( m_Prop_line );
}

constexpr std::tuple<ALife::EHitType, LPCSTR, LPCSTR, float, bool, LPCSTR> af_immunity[] =
{
    //{ ALife::eHitType,			"section",                  "caption",                                  magnitude, sign_inverse, "unit" }
    { ALife::eHitTypeBurn,			"burn_immunity",            "ui_inv_outfit_burn_protection",            100.0f,      false,        "%" },
    { ALife::eHitTypeShock,			"shock_immunity",           "ui_inv_outfit_shock_protection",           100.0f,      false,        "%" },
    { ALife::eHitTypeChemicalBurn,	"chemical_burn_immunity",   "ui_inv_outfit_chemical_burn_protection",   100.0f,      false,        "%" },
    { ALife::eHitTypeRadiation,		"radiation_immunity",       "ui_inv_outfit_radiation_protection",       -100.0f,     true,        "%" },
    { ALife::eHitTypeTelepatic,		"telepatic_immunity",       "ui_inv_outfit_telepatic_protection",       100.0f,      false,        "%" },
    { ALife::eHitTypeWound,			"wound_immunity",           "ui_inv_outfit_wound_protection",           100.0f,      false,        "%" },
    { ALife::eHitTypeFireWound,		"fire_wound_immunity",      "ui_inv_outfit_fire_wound_protection",      100.0f,      false,        "%" },
    { ALife::eHitTypeStrike,		"strike_immunity",          "ui_inv_outfit_strike_protection",          100.0f,      false,        "%" },
    { ALife::eHitTypeExplosion,		"explosion_immunity",       "ui_inv_outfit_explosion_protection",       100.0f,      false,        "%" },
};

constexpr std::tuple<ALife::EConditionRestoreType, LPCSTR, LPCSTR, float, bool, LPCSTR> af_restore[] =
{
	//{ ALife::EConditionRestoreType,   "section",                  "caption",          magnitude, sign_inverse, "unit" }
	{ ALife::eHealthRestoreSpeed,       "health_restore_speed",     "ui_inv_health",    100.0f,    false,        "%" },
	{ ALife::eSatietyRestoreSpeed,      "satiety_restore_speed",    "ui_inv_satiety",   100.0f,    false,        "%" },
	{ ALife::ePowerRestoreSpeed,        "power_restore_speed",      "ui_inv_power",     1.0f,      false,        nullptr },
	{ ALife::eThirstRestoreSpeed,       "thirst_restore_speed",     "ui_inv_thirst",     1.0f,     false,        nullptr },
	{ ALife::eBleedingRestoreSpeed,     "bleeding_restore_speed",   "ui_inv_bleeding",  100.0f,    false,        "%" },
	{ ALife::eRadiationRestoreSpeed,    "radiation_restore_speed",  "ui_inv_radiation", 1.0f,	    true,        nullptr },
};

LPCSTR af_actor_param_names[] = 
{
	"satiety_health_v",
	"satiety_v",
	"satiety_power_v",
	"wound_incarnation_v",
	"radiation_v",
};

static_assert(std::size(af_restore) == ALife::eRestoreTypeMax,
    "All restore types should be listed in the tuple above.");

LPCSTR af_params = "af_params";
void CUIArtefactParams::InitFromXml( CUIXml& xml )
{
	XML_NODE* stored_root = xml.GetLocalRoot();
	XML_NODE* base_node   = xml.NavigateToNode( af_params, 0 );
	if ( !base_node )
	{
		return;
	}
	CUIXmlInit::InitWindow( xml, af_params, 0, this );
	xml.SetLocalRoot( base_node );
	
	if (xml.NavigateToNode("prop_line"))
	{
		m_Prop_line = UIHelper::CreateStatic(xml, "prop_line", this);
		m_Prop_line->SetAutoDelete(false);
	}

	LPCSTR name;
	if (xml.NavigateToNode("condition"))
	{
		m_disp_condition = CreateItem(xml, "condition", "st_condition");
	}
   	for (auto [id, section, caption, magnitude, sign_inverse, unit] : af_immunity)
    {
        m_immunity_item[id] = CreateItem(xml, section, magnitude, sign_inverse, unit, caption);
    }
    for (auto [id, section, caption, magnitude, sign_inverse, unit] : af_restore)
    {
        m_restore_item[id] = CreateItem(xml, section, magnitude, sign_inverse, unit, caption);
    }
	
	if (xml.NavigateToNode("af_slots"))
	{
		m_af_slots = CreateItem(xml, "af_slots", "st_prop_artefact");
	}
	m_additional_weight = CreateItem(xml, "additional_weight", "ui_inv_weight", "ui_inv_outfit_additional_weight");

	xml.SetLocalRoot( stored_root );
}

UIArtefactParamItem* CUIArtefactParams::CreateItem(CUIXml& uiXml, pcstr section,
    float magnitude, bool isSignInverse, const shared_str& unit,
    shared_str translationId, shared_str translationId2 /*= nullptr*/)
{
	UIArtefactParamItem* item = new UIArtefactParamItem();

	const UIArtefactParamItem::InitResult result = item->Init(uiXml, section);
	switch (result)
	{
	case UIArtefactParamItem::InitResult::Failed:
		xr_delete(item);
		return nullptr;

	case UIArtefactParamItem::InitResult::Plain:
		item->SetDefaultValuesPlain(magnitude, isSignInverse, unit);
		break;
	}

	// use either translationId or translationId2
	// but set translationId if both unavailable
	shared_str name = g_pStringTable->translate(translationId);
	shared_str name2 = translationId2 != nullptr ? g_pStringTable->translate(translationId2) : nullptr;

	if (name != translationId && name2 != translationId2)
		item->SetCaption(name2.c_str());
	else
		item->SetCaption(name.c_str());

	item->SetAutoDelete(false);
	return item;
}

UIArtefactParamItem* CUIArtefactParams::CreateItem(CUIXml& uiXml, pcstr section,
	shared_str translationId, shared_str translationId2 /*= nullptr*/)
{
	return CreateItem(uiXml, section, 1.0f, false, nullptr, translationId, translationId2);
}

bool CUIArtefactParams::Check(const shared_str& af_section)
{
	return !!pSettings->line_exist(af_section, "af_actor_properties");
}

void CUIArtefactParams::SetInfo(CInventoryItem& pInvItem)
{
	DetachAll();
	if (m_Prop_line)
		AttachChild( m_Prop_line );

	CActor* actor = Level().CurrentViewEntity()->cast_actor();
	if (!actor)
	{
		return;
	}

	float val = 0.0f, max_val = 1.0f, h = 0.0f;
	Fvector2 pos {0,0};
	if (m_Prop_line)
		h = m_Prop_line->GetWndPos().y + m_Prop_line->GetWndSize().y;

	const auto setValue = [&](UIArtefactParamItem* item, float value)
    {
        item->SetValue(value);

        Fvector2 pos = item->GetWndPos();
        pos.y = h;
        item->SetWndPos(pos);

        h += item->GetWndSize().y;
        AttachChild(item);
    };

	if (m_disp_condition && is_artefact() && static_cast<CArtefact*>(&pInvItem)->DegradationRate())
	{
		setValue(m_disp_condition, pInvItem.GetCondition());
	}

	const shared_str& af_section = pInvItem.m_section_id.c_str();

	if (is_artefact())
	{
		for (auto [id, immunity_section, immunity_caption, magnitude, sign_inverse, unit] : af_immunity)
		{
			if (!m_immunity_item[id])
				continue;

			shared_str const& hit_absorbation_sect = pSettings->r_string(af_section, "hit_absorbation_sect");
			val = pSettings->r_float(hit_absorbation_sect, immunity_section);
			if ((!m_immunity_item[id]->GetLegacyMode() && fis_zero(val)) 
				|| (m_immunity_item[id]->GetLegacyMode() && fsimilar(val, 1.0f)))
			{
				continue;
			}
			if (!m_immunity_item[id]->GetLegacyMode())
			{
				max_val = actor->conditions().GetZoneMaxPower(id);
				val /= max_val;
			}
			else
			{
				val = (1.0f - val);
			}
			setValue(m_immunity_item[id], val * pInvItem.GetCondition());
		}

		for (auto [id, restore_section, restore_caption, magnitude, sign_inverse, unit] : af_restore)
		{
			if (!m_restore_item[id])
				continue;

			float actor_val = pSettings->r_float("actor_condition", af_actor_param_names[id]);
			val = pSettings->r_float(af_section, restore_section);
			if (fis_zero(val))
			{
				continue;
			}
			if (m_restore_item[id]->GetLegacyMode())
			{
				val = (val/actor_val);
			}
			setValue(m_restore_item[id], val * pInvItem.GetCondition());
		}
	}
	else if (!is_backpack())
	{
		u32 count = READ_IF_EXISTS(pSettings, r_u32, af_section, "artefact_count", 0);
		if (count > 0 && m_af_slots)
		{
			setValue(m_af_slots, count);
		}
	}

	if (m_additional_weight)
	{
		val	= READ_IF_EXISTS(pSettings, r_float, af_section, "additional_inventory_weight", 0.0f);
		if ( !fis_zero(val) )
		{
			setValue(m_additional_weight, val * (is_artefact() ? pInvItem.GetCondition() : 1));
		}
	}

	SetHeight( h );
}

/// ----------------------------------------------------------------

UIArtefactParamItem::UIArtefactParamItem()
{
	m_caption   = nullptr;
	m_value     = nullptr;
	m_magnitude = 1.0f;
	m_sign_inverse = false;
	
	m_unit_str._set( "" );
	m_texture_minus._set( "" );
	m_texture_plus._set( "" );
	m_text_legacy = nullptr;
}

UIArtefactParamItem::~UIArtefactParamItem()
{
}

UIArtefactParamItem::InitResult UIArtefactParamItem::Init(CUIXml& xml, pcstr section)
{
	if(!CUIXmlInit::InitStatic(xml, section, 0, this, false))
		return InitPlain(xml, section);

	XML_NODE* base_node = xml.GetLocalRoot();

	xml.SetLocalRoot( xml.NavigateToNode( section ) );

	m_caption   = UIHelper::CreateStatic( xml, "caption", this );
	m_value     = UIHelper::CreateStatic( xml, "value",   this );
	m_magnitude = xml.ReadAttribFlt( "value", 0, "magnitude", 1.0f );
	m_sign_inverse = (xml.ReadAttribInt( "value", 0, "sign_inverse", 0 ) == 1);
	
	LPCSTR unit_str = xml.ReadAttrib( "value", 0, "unit_str", "" );
	m_unit_str._set( g_pStringTable->translate( unit_str ) );
	
	LPCSTR texture_minus = xml.Read( "texture_minus", 0, "" );
	if ( texture_minus && xr_strlen(texture_minus) )
	{
		m_texture_minus._set( texture_minus );
		
		LPCSTR texture_plus = xml.Read( "caption:texture", 0, "" );
		m_texture_plus._set( texture_plus );
		VERIFY( m_texture_plus.size() );
	}
	xml.SetLocalRoot(base_node);
	return InitResult::Normal;
}

UIArtefactParamItem::InitResult UIArtefactParamItem::InitPlain(CUIXml& xml, pcstr section)
{
    string256 buf;
    xr_strconcat(buf, "static_", section);
	if (!CUIXmlInit::InitStatic(xml, buf, 0, this, false))
	{
		return InitResult::Failed;
	}
    m_caption = new CUIStatic();
    m_caption->SetAutoDelete(true);
    AttachChild(m_caption);
    m_caption->Show(false); // hack

    m_value = new CUIStatic();
    m_value->SetAutoDelete(true);
    AttachChild(m_value);
    m_value->Show(false); // hack

	TextOff();
	m_text_legacy = UIHelper::CreateStatic(xml, buf, this);
	m_text_legacy->TextureOff();
	m_text_legacy->SetWidth(1000.f); // St4lker0k765: hack for complex mode

    return InitResult::Plain;
}

void UIArtefactParamItem::SetDefaultValuesPlain(float magnitude, bool isSignInverse, const shared_str& unit)
{
    m_magnitude = magnitude;
    m_sign_inverse = isSignInverse;
    m_unit_str = unit;
}
void UIArtefactParamItem::SetCaption( LPCSTR name )
{
	m_caption->TextItemControl()->SetText( name );
}

void UIArtefactParamItem::SetValue( float value )
{
	value *= m_magnitude;
	string32	buf;
	xr_sprintf( buf, "%+.0f", value );
	
	string256 str;
	if ( m_unit_str.size() )
	{
		xr_strconcat( str, buf, " ", m_unit_str.c_str() );
	}
	else
	{
		xr_strconcat( str, buf );
	}
	m_value->SetText( str );

	constexpr u32 red_clr = color_argb(255, 210, 50, 50);
	constexpr u32 green_clr = color_argb(255, 170, 170, 170);

	bool positive = (value >= 0.0f);
	positive      = (m_sign_inverse)? !positive : positive;
	u32 color     = (positive      )? green_clr : red_clr;
	m_value->SetTextColor( color );

	if ( m_texture_minus.size() )
	{
		if ( positive )
		{
			m_caption->InitTexture( m_texture_plus.c_str() );
		}
		else
		{
			m_caption->InitTexture( m_texture_minus.c_str() );
		}
	}

	// hack
	if (!m_caption->IsShown() && !m_value->IsShown())
	{
		xr_sprintf(buf, "%s %s %s", m_caption->GetText(), positive ? "%c[green]" : "%c[red]", m_value->GetText());
		m_text_legacy->SetText(buf);
	}
}
