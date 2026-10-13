#include "StdAfx.h"
#include "UIBoosterInfo.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "object_broker.h"
#include "../EntityCondition.h"
#include "../Actor.h"
#include "../ActorCondition.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrEngine/string_table.h"
#include "../../xrCore/Kernel/EngineExternal.h"

CUIBoosterInfo::CUIBoosterInfo()
{
	for(u32 i = 0; i < eBoostExplImmunity; ++i)
	{
		m_booster_items[i] = nullptr;
	}
	m_booster_satiety = nullptr;
	m_booster_thirst = nullptr;
	m_booster_sleepiness = nullptr;
	m_booster_intoxication = nullptr;
	m_booster_anabiotic = nullptr;
	m_booster_time = nullptr;
}

CUIBoosterInfo::~CUIBoosterInfo()
{
	delete_data(m_booster_items);
	xr_delete(m_booster_satiety);
	xr_delete(m_booster_thirst);
	xr_delete(m_booster_sleepiness);
	xr_delete(m_booster_intoxication);
	xr_delete(m_booster_anabiotic);
	xr_delete(m_booster_time);
	if (m_Prop_line)
		xr_delete(m_Prop_line);
}

const char* boost_influence_caption[] =
{
	"ui_inv_health",
	"ui_inv_power",
	"ui_inv_radiation",
	"ui_inv_bleeding",
	"ui_inv_outfit_additional_weight",
	"ui_inv_outfit_radiation_protection",
	"ui_inv_outfit_telepatic_protection",
	"ui_inv_outfit_chemical_burn_protection",
	"ui_inv_outfit_burn_immunity",
	"ui_inv_outfit_shock_immunity",
	"ui_inv_outfit_radiation_immunity",
	"ui_inv_outfit_telepatic_immunity",
	"ui_inv_outfit_chemical_burn_immunity"
};

void CUIBoosterInfo::InitFromXml(CUIXml& xml)
{
	const char* base	= "booster_params";
	XML_NODE* stored_root = xml.GetLocalRoot();
	XML_NODE* base_node   = xml.NavigateToNode( base, 0 );
	if(!base_node)
		return;

	CUIXmlInit::InitWindow(xml, base, 0, this);
	xml.SetLocalRoot(base_node);
	
	if (xml.NavigateToNode("prop_line"))
	{
		m_Prop_line = UIHelper::CreateStatic(xml, "prop_line", this);
		m_Prop_line->SetAutoDelete(false);
	}

	for(u32 i = 0; i < eBoostExplImmunity; ++i)
	{
		if (xml.NavigateToNode(ef_boosters_section_names[i]))
		{
			m_booster_items[i] = new UIBoosterInfoItem();
			m_booster_items[i]->Init(xml, ef_boosters_section_names[i]);
			m_booster_items[i]->SetAutoDelete(false);

			const char* name = g_pStringTable->translate(boost_influence_caption[i]).c_str();
			m_booster_items[i]->SetCaption(name);

			xml.SetLocalRoot(base_node);
		}
	}

	m_booster_satiety = new UIBoosterInfoItem();
	m_booster_satiety->Init(xml, "boost_satiety");
	m_booster_satiety->SetAutoDelete(false);
	const char* name = g_pStringTable->translate("ui_inv_satiety").c_str();
	m_booster_satiety->SetCaption(name);
	xml.SetLocalRoot( base_node );

	if (xml.NavigateToNode("boost_thirst"))
	{
		m_booster_thirst = new UIBoosterInfoItem();
		m_booster_thirst->Init(xml, "boost_thirst");
		m_booster_thirst->SetAutoDelete(false);
		m_booster_thirst->SetCaption(g_pStringTable->translate("ui_inv_thirst").c_str());
		xml.SetLocalRoot(base_node);
	}

	if (xml.NavigateToNode("boost_sleepiness"))
	{
		m_booster_sleepiness = new UIBoosterInfoItem();
		m_booster_sleepiness->Init(xml, "boost_sleepiness");
		m_booster_sleepiness->SetAutoDelete(false);
		m_booster_sleepiness->SetCaption(g_pStringTable->translate("ui_inv_sleepiness").c_str());
		xml.SetLocalRoot(base_node);
	}

	const static bool enableMedIntoxication = EngineExternal()[EEngineExternalGame::EnableMedIntoxication];
	if (enableMedIntoxication && xml.NavigateToNode("boost_intoxication"))
	{
		m_booster_intoxication = new UIBoosterInfoItem();
		m_booster_intoxication->Init(xml, "boost_intoxication");
		m_booster_intoxication->SetAutoDelete(false);
		m_booster_intoxication->SetCaption("");
		m_booster_intoxication->ShowValue(false);
		xml.SetLocalRoot(base_node);
	}

	if (xml.NavigateToNode("boost_anabiotic"))
	{
		m_booster_anabiotic = new UIBoosterInfoItem();
		m_booster_anabiotic->Init(xml, "boost_anabiotic");
		m_booster_anabiotic->SetAutoDelete(false);
		name = g_pStringTable->translate("ui_inv_survive_surge").c_str();
		m_booster_anabiotic->SetCaption(name);
		xml.SetLocalRoot(base_node);
	}

	if (xml.NavigateToNode("boost_time"))
	{
		m_booster_time = new UIBoosterInfoItem();
		m_booster_time->Init(xml, "boost_time");
		m_booster_time->SetAutoDelete(false);
		name = g_pStringTable->translate("ui_inv_effect_time").c_str();
		m_booster_time->SetCaption(name);
	}

	xml.SetLocalRoot( stored_root );
}

void CUIBoosterInfo::SetInfo( shared_str const& section )
{
	DetachAll();
	if (m_Prop_line)
		AttachChild( m_Prop_line );

	CActor* actor = Level().CurrentViewEntity()->cast_actor();
	if (!actor)
	{
		return;
	}

	CEntityCondition::BOOSTER_MAP boosters = actor->conditions().GetCurBoosterInfluences();

	float val = 0.0f, max_val = 1.0f, h = 0.0f;
	Fvector2 pos;
	if (m_Prop_line)
		h = m_Prop_line->GetWndPos().y + m_Prop_line->GetWndSize().y;

	for (u32 i = 0; i < eBoostExplImmunity; ++i)
	{
		if(pSettings->line_exist(section.c_str(), ef_boosters_section_names[i]) && ef_boosters_section_names[i] && m_booster_items[i])
		{
			val = pSettings->r_float(section, ef_boosters_section_names[i]) * m_booster_items[i]->m_magnitude;
			if (std::abs(val) < 1.f)
			{
				continue;
			}

			EBoostParams type = (EBoostParams)i;
			switch(type)
			{
				case eBoostHpRestore: 
				case eBoostPowerRestore: 
				case eBoostBleedingRestore: 
				case eBoostMaxWeight: 
					max_val = 1.0f;
					break;
				case eBoostRadiationRestore: 
					max_val = -1.0f;
					break;
				case eBoostBurnImmunity: 
					max_val = actor->conditions().GetZoneMaxPower(ALife::infl_fire);
					break;
				case eBoostShockImmunity: 
					max_val = actor->conditions().GetZoneMaxPower(ALife::infl_electra);
					break;
				case eBoostRadiationImmunity: 
				case eBoostRadiationProtection: 
					max_val = actor->conditions().GetZoneMaxPower(ALife::infl_rad);
					break;
				case eBoostTelepaticImmunity: 
				case eBoostTelepaticProtection: 
					max_val = actor->conditions().GetZoneMaxPower(ALife::infl_psi);
					break;
				case eBoostChemicalBurnImmunity: 
				case eBoostChemicalBurnProtection: 
					max_val = actor->conditions().GetZoneMaxPower(ALife::infl_acid);
					break;
			}
			val /= max_val;
			m_booster_items[i]->SetValue(val);

			pos.set(m_booster_items[i]->GetWndPos());
			pos.y = h;
			m_booster_items[i]->SetWndPos(pos);

			h += m_booster_items[i]->GetWndSize().y;
			AttachChild(m_booster_items[i]);
		}
	}

	if(pSettings->line_exist(section.c_str(), "eat_satiety"))
	{
		val = pSettings->r_float(section, "eat_satiety") * m_booster_satiety->m_magnitude;
		if (std::abs(val) >= 1.f)
		{
			m_booster_satiety->SetValue(val);
			pos.set(m_booster_satiety->GetWndPos());
			pos.y = h;
			m_booster_satiety->SetWndPos(pos);

			h += m_booster_satiety->GetWndSize().y;
			AttachChild(m_booster_satiety);
		}
	}
	if (pSettings->line_exist(section.c_str(), "eat_thirst") && m_booster_thirst)
	{
		val = pSettings->r_float(section, "eat_thirst") * m_booster_thirst->m_magnitude;
		if (std::abs(val) >= 1.f)
		{
			m_booster_thirst->SetValue(val);
			pos.set(m_booster_thirst->GetWndPos());
			pos.y = h;
			m_booster_thirst->SetWndPos(pos);

			h += m_booster_thirst->GetWndSize().y;
			AttachChild(m_booster_thirst);
		}
	}


	if(pSettings->line_exist(section.c_str(), "eat_sleepiness") && m_booster_sleepiness)
	{
		val = pSettings->r_float(section, "eat_sleepiness") * m_booster_sleepiness->m_magnitude;
		if (std::abs(val) >= 1.f)
		{
			m_booster_sleepiness->SetValue(val);
			pos.set(m_booster_sleepiness->GetWndPos());
			pos.y = h;
			m_booster_sleepiness->SetWndPos(pos);

			h += m_booster_sleepiness->GetWndSize().y;
			AttachChild(m_booster_sleepiness);
		}
	}

	if (pSettings->line_exist(section.c_str(), "eat_intoxication") && m_booster_intoxication)
	{
		val = pSettings->r_float(section, "eat_intoxication");
		if (!fis_zero(val))
		{
			const char* descId = "ui_inv_intoxication_raise";
			if (val <= -0.9f)
			{
				descId = "ui_inv_intoxication_clear";
			}
			else if (val < 0.0f)
			{
				descId = "ui_inv_intoxication_reduce";
			}
			else if (val <= 0.12f)
			{
				descId = "ui_inv_intoxication_raise_light";
			}
			else if (val <= 0.20f)
			{
				descId = "ui_inv_intoxication_raise";
			}
			else
			{
				descId = "ui_inv_intoxication_raise_heavy";
			}

			m_booster_intoxication->SetCaption(g_pStringTable->translate(descId).c_str());
			m_booster_intoxication->ShowValue(false);
			pos.set(m_booster_intoxication->GetWndPos());
			pos.y = h;
			m_booster_intoxication->SetWndPos(pos);

			h += m_booster_intoxication->GetWndSize().y;
			AttachChild(m_booster_intoxication);
		}
	}

	if(!xr_strcmp(section.c_str(), "drug_anabiotic") && m_booster_anabiotic)
	{
		pos.set(m_booster_anabiotic->GetWndPos());
		pos.y = h;
		m_booster_anabiotic->SetWndPos(pos);

		h += m_booster_anabiotic->GetWndSize().y;
		AttachChild(m_booster_anabiotic);
	}

	if(pSettings->line_exist(section.c_str(), "boost_time") && m_booster_time)
	{
		val	= pSettings->r_float(section, "boost_time");
		if(!fis_zero(val))
		{
			m_booster_time->SetValue(val * m_booster_time->m_magnitude);
			pos.set(m_booster_time->GetWndPos());
			pos.y = h;
			m_booster_time->SetWndPos(pos);

			h += m_booster_time->GetWndSize().y;
			AttachChild(m_booster_time);
		}
	}
	SetHeight(h);
}
