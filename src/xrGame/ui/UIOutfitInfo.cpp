#include "StdAfx.h"
#include "UIOutfitInfo.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UIDoubleProgressBar.h"
#include "UIHelperGame.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../CustomOutfit.h"
#include "../ActorHelmet.h"
#include "../Actor.h"
#include "../ActorCondition.h"
#include "../player_hud.h"
#include "../../xrEngine/string_table.h"
#include "../BoneProtections.h"

constexpr std::tuple<ALife::EHitType, const char*, const char*, float, const char*> outfit_immunity[] =
{
	{ ALife::eHitTypeBurn,			"burn_immunity",            "ui_inv_outfit_burn_protection",            100.0f,      "%" },
	{ ALife::eHitTypeShock,			"shock_immunity",           "ui_inv_outfit_shock_protection",           100.0f,      "%" },
	{ ALife::eHitTypeChemicalBurn,	"chemical_burn_immunity",   "ui_inv_outfit_chemical_burn_protection",   100.0f,      "%" },
	{ ALife::eHitTypeRadiation,		"radiation_immunity",       "ui_inv_outfit_radiation_protection",       100.0f,		 "%" },
	{ ALife::eHitTypeTelepatic,		"telepatic_immunity",       "ui_inv_outfit_telepatic_protection",       100.0f,      "%" },
	{ ALife::eHitTypeWound,			"wound_immunity",           "ui_inv_outfit_wound_protection",           100.0f,      "%" },
	{ ALife::eHitTypeFireWound,		"fire_wound_immunity",      "ui_inv_outfit_fire_wound_protection",      100.0f,      "%" },
	{ ALife::eHitTypeStrike,		"strike_immunity",          "ui_inv_outfit_strike_protection",          100.0f,      "%" },
	{ ALife::eHitTypeExplosion,		"explosion_immunity",       "ui_inv_outfit_explosion_protection",       100.0f,      "%" },
};

CUIOutfitImmunity::CUIOutfitImmunity()
{
	m_unit_str._set("");
	m_value = nullptr;
	m_magnitude = 1.0f;
	m_name = nullptr;
	m_progress = nullptr;
	m_text_legacy = nullptr;
}

CUIOutfitImmunity::~CUIOutfitImmunity()
{
}

const char* outfit_info = "outfit_info";
CUIOutfitImmunity::InitResult CUIOutfitImmunity::Init( CUIXml& xml_doc, const char* section )
{
	if(!CUIXmlInit::InitStatic(xml_doc, section, 0, this, false))
		return InitPlain(xml_doc, section);

	m_name = UIHelper::CreateStatic(xml_doc, section, this);
	m_name->TextureOff();
	
	XML_NODE* base_node = xml_doc.GetLocalRoot();
	xml_doc.SetLocalRoot(xml_doc.NavigateToNode(section));


	if (xml_doc.NavigateToNode("progress_immunity"))
	{
		m_progress = new CUIDoubleProgressBar();
		AttachChild(m_progress);
		m_progress->InitFromXml(xml_doc, "progress_immunity");
	}
	
	m_value = UIHelper::CreateStatic(xml_doc, "static_value", this);

	m_magnitude = xml_doc.ReadAttribFlt( "static_value", 0, "magnitude", 1.0f);

	const char* unit_str = xml_doc.ReadAttrib("static_value", 0, "unit_str", "");
	m_unit_str._set(g_pStringTable->translate(unit_str));

	xml_doc.SetLocalRoot(base_node);
	return InitResult::Normal;
}

CUIOutfitImmunity::InitResult CUIOutfitImmunity::InitPlain(CUIXml& xml, const char* section)
{
    string256 buf;
    xr_strconcat(buf, "static_", section);
	if (!CUIXmlInit::InitStatic(xml, buf, 0, this, false))
	{
		return InitResult::Failed;
	}
	m_name = new CUIStatic();
	m_name->SetAutoDelete(true);
	AttachChild(m_name);
	m_name->Show(false); // hack

    m_value = new CUIStatic();
    m_value->SetAutoDelete(true);
    AttachChild(m_value);
    m_value->Show(false); // hack

	TextOff();
	m_text_legacy = UIHelper::CreateStatic(xml, buf, this);
	m_text_legacy->TextureOff();
	m_text_legacy->SetWidth(1000.f); // St4lker0k765: hack for complex mode

	m_legacy_mode = true;

    return InitResult::Plain;
}

void CUIOutfitImmunity::SetProgressValue(float cur, float comp)
{
	cur *= m_magnitude;
	comp *= m_magnitude;
	if (m_progress)
		m_progress->SetTwoPos(cur, comp);

	string32 buf;
	xr_sprintf(buf, "%.0f", cur);

	string256 str;
	if (m_unit_str.size())
		xr_strconcat(str, buf, m_unit_str.c_str());
	else
		xr_strconcat(str, buf);

	m_value->SetText(str);

	// hack
	if (m_legacy_mode)
	{
		int sz = xr_sprintf(str, "%s %s %+3.0f%%", m_name->GetText(), (cur > 0.0f) ? "%c[green]" : "%c[red]", cur);
		if (!fis_zero(m_af_value))
		{
			sz += xr_sprintf(str + sz, sizeof(str) - sz, " %s %+3.0f%%", (m_af_value > 0.0f) ? "%c[green]" : "%c[red]", m_af_value*100.f);
		}

		m_text_legacy->SetText(str);
	}
}


void CUIOutfitImmunity::SetDefaultValuesPlain(float magnitude, const shared_str& unit)
{
	m_magnitude = magnitude;
	m_unit_str = unit;
}

void CUIOutfitImmunity::SetCaption(const char* name)
{
	m_name->TextItemControl()->SetText(name);
}

// ===========================================================================================

CUIOutfitInfo::CUIOutfitInfo()
{
	m_Prop_line = nullptr;
	m_listWnd = nullptr;
	for ( u32 i = 0; i < max_count; ++i )
	{
		m_items[i] = nullptr;
	}
}

CUIOutfitInfo::~CUIOutfitInfo()
{
	for ( u32 i = 0; i < max_count; ++i )
	{
		xr_delete( m_items[i] );
	}
}

void CUIOutfitInfo::InitFromXml( CUIXml& xml_doc )
{
	XML_NODE* stored_root = xml_doc.GetLocalRoot();
	XML_NODE* base_node = xml_doc.NavigateToNode(outfit_info, 0);
	if (!base_node)
	{
		return;
	}
	CUIXmlInit::InitWindow( xml_doc, outfit_info, 0, this );
	xml_doc.SetLocalRoot(base_node);
	
	string256 buf;

	m_listWnd = UIHelper::CreateScrollView(xml_doc, "scroll_view", this, false);

	m_caption = UIHelper::CreateStatic(xml_doc, "caption", this, false);

	m_Prop_line = UIHelper::CreateStatic(xml_doc, "prop_line", this, false);

	Fvector2 pos;
	if (m_Prop_line)
		pos.set(0.0f, m_Prop_line->GetWndPos().y + m_Prop_line->GetWndSize().y);
	else if (m_caption)
		pos.set(0.0f, m_caption->GetWndSize().y);

	for (auto [id, section, caption, magnitude, unit] : outfit_immunity)
	{
		m_items[id] = CreateItem(xml_doc, section, magnitude, unit, caption);
		if (m_items[id] && !m_listWnd)
		{
			AttachChild(m_items[id]);
			m_items[id]->SetWndPos(pos);
			pos.y += m_items[id]->GetWndSize().y;
		}
	}
	pos.x = GetWndSize().x;
	SetWndSize( pos );
	xml_doc.SetLocalRoot(stored_root);
}

CUIOutfitImmunity* CUIOutfitInfo::CreateItem(CUIXml& uiXml, const char* section,
    float magnitude, const shared_str& unit,
    shared_str translationId)
{
	CUIOutfitImmunity* item = new CUIOutfitImmunity();

	const CUIOutfitImmunity::InitResult result = item->Init(uiXml, section);
	switch (result)
	{
	case CUIOutfitImmunity::InitResult::Failed:
		xr_delete(item);
		return nullptr;

	case CUIOutfitImmunity::InitResult::Plain:
		item->SetDefaultValuesPlain(magnitude, unit);
		break;
	}

	item->SetCaption(g_pStringTable->translate(translationId).c_str());

	item->SetAutoDelete(false);
	return item;
}

void CUIOutfitInfo::UpdateInfo(CCustomOutfit* cur_outfit, CCustomOutfit* slot_outfit)
{
	CActor* actor = Level().CurrentViewEntity()->cast_actor();
	if ( !actor || !cur_outfit )
	{
		for (u32 i = 0; i < max_count; ++i)
		{
			if (m_items[i] && m_items[i]->GetLegacyMode())
			{
				float _val_af = Actor()->HitArtefactsOnBeltLegacy(1.0f, (ALife::EHitType)i);
				_val_af = 1.0f - _val_af;

				m_items[i]->SetAfValue(_val_af);
				m_items[i]->SetProgressValue(0.0f, 0.0f);

				if (!m_items[i]->GetParent() && !fis_zero(_val_af))
				{
					m_listWnd->AddWindow(m_items[i], false);
				}
				else if (m_items[i]->GetParent() && fis_zero(_val_af))
					m_listWnd->RemoveWindow(m_items[i]);
			}
		}
		return;
	}
	for ( u32 i = 0; i < max_count; ++i )
	{	
		if ( !m_items[i] || (i == ALife::eHitTypeFireWound && !m_items[ALife::eHitTypeFireWound]->GetLegacyMode()))
		{
			continue;
		}
		
		ALife::EHitType hit_type = (ALife::EHitType)i;
		float max_power = actor->conditions().GetZoneMaxPower( hit_type );

		float cur = cur_outfit->GetDefHitTypeProtection( hit_type );
		cur /= max_power; // = 0..1
		if (cur_outfit->m_boneProtection->m_hitFracType == SBoneProtections::HitFraction)
			cur = 1 - cur;
		float slot = cur;
		
		if ( slot_outfit )
		{
			slot = slot_outfit->GetDefHitTypeProtection( hit_type );
			slot /= max_power; //  = 0..1
			if (slot_outfit->m_boneProtection->m_hitFracType == SBoneProtections::HitFraction)
			{
				slot = 1 - slot;
			}
		}

		float _val_af = Actor()->HitArtefactsOnBeltLegacy(1.0f, hit_type);
		_val_af = 1.0f - _val_af;

		m_items[i]->SetAfValue(_val_af);
		m_items[i]->SetProgressValue( cur, slot );

		if (m_listWnd)
		{
			if (m_items[i]->GetParent())
				m_listWnd->RemoveWindow(m_items[i]);

			if (!fis_zero(_val_af) || !fis_zero(cur))
				m_listWnd->AddWindow(m_items[i], false);
		}
	}

	if ( m_items[ALife::eHitTypeFireWound] && !m_items[ALife::eHitTypeFireWound]->GetLegacyMode() )
	{
		IKinematics* ikv = PKinematics(actor->Visual());
		VERIFY( ikv );
		u16 spine_bone = ikv->LL_BoneID( "bip01_spine" );

		float cur = cur_outfit->GetBoneArmor( spine_bone )*cur_outfit->GetCondition();
		float slot = cur;
		if(slot_outfit)
		{
			spine_bone = ikv->LL_BoneID( "bip01_spine" );
			slot = slot_outfit->GetBoneArmor( spine_bone )*slot_outfit->GetCondition(); 
		}
		float max_power = actor->conditions().GetMaxFireWoundProtection();
		cur /= max_power;
		slot /= max_power;

		float _val_af = Actor()->HitArtefactsOnBeltLegacy(1.0f, ALife::eHitTypeFireWound);
		_val_af = 1.0f - _val_af;

		m_items[ALife::eHitTypeFireWound]->SetAfValue(_val_af);
		m_items[ALife::eHitTypeFireWound]->SetProgressValue( cur, slot );

		if (m_listWnd)
		{
			if (m_items[ALife::eHitTypeFireWound]->GetParent())
				m_listWnd->RemoveWindow(m_items[ALife::eHitTypeFireWound]);

			if (!fis_zero(_val_af) || !fis_zero(cur))
				m_listWnd->AddWindow(m_items[ALife::eHitTypeFireWound], false);
		}
	}
}


void CUIOutfitInfo::UpdateInfo(CHelmet* cur_helmet, CHelmet* slot_helmet)
{
	CActor* actor = Level().CurrentViewEntity()->cast_actor();
	if ( !actor || !cur_helmet )
	{
		return;
	}

	for ( u32 i = 0; i < max_count; ++i )
	{
		if ( i == ALife::eHitTypeFireWound || !m_items[i] )
		{
			continue;
		}
		
		ALife::EHitType hit_type = (ALife::EHitType)i;
		float max_power = actor->conditions().GetZoneMaxPower( hit_type );

		float cur = cur_helmet->GetDefHitTypeProtection( hit_type );
		cur /= max_power; // = 0..1
		float slot = cur;
		
		if ( slot_helmet )
		{
			slot = slot_helmet->GetDefHitTypeProtection( hit_type );
			slot /= max_power; //  = 0..1
		}
		m_items[i]->SetProgressValue( cur, slot );
	}

	if ( m_items[ALife::eHitTypeFireWound] )
	{
		IKinematics* ikv = PKinematics(actor->Visual());
		VERIFY( ikv );
		u16 spine_bone = ikv->LL_BoneID( "bip01_head" );

		float cur = cur_helmet->GetBoneArmor( spine_bone )*cur_helmet->GetCondition();
		float slot = (slot_helmet)? slot_helmet->GetBoneArmor( spine_bone )*slot_helmet->GetCondition() : cur;
		
		m_items[ALife::eHitTypeFireWound]->SetProgressValue( cur, slot );
	}

}
