////////////////////////////////////////////////////////////////////////////
//	Module 		: UIRankingWnd.cpp
//	Created 	: 17.01.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Ranking window class implementation
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "pch_script.h"
#include "UIRankingWnd.h"
#include "PdaUiSound.h"

#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIStackPanel.h"
#include "UIInventoryUtilities.h"
#include "PdaConstants.h"
#include "PdaScriptBridge.h"

#include "../Actor.h"
#include "../ai_space.h"
#include "../alife_simulator.h"
#include "../HudPdaAnimator.h"
#include "../../xrScripts/script_engine.h"
#include "../character_community.h"
#include "../character_reputation.h"
#include "../relation_registry.h"
#include "../../xrEngine/string_table.h"
#include "UICharacterInfo.h"
#include "../../xrUI/ui_base.h"

using namespace luabind;

namespace
{
constexpr u32 rankingStatActorMoneyEarnedIndex = 7;
constexpr u32 rankingStatActorMoneySpentIndex = 8;
constexpr u32 rankingStatActorHelpWoundedIndex = 9;
constexpr u32 rankingStatActorHeadshotsIndex = 10;
constexpr u32 rankingStatActorDeathsIndex = 11;
constexpr u32 rankingStatActorDistanceIndex = 12;

bool RankingStatIdMatches(const shared_str& statId, const char* canonicalId)
{
	if (statId.size() == 0)
	{
		return false;
	}

	if (statId == canonicalId)
	{
		return true;
	}

	if (xr_strcmp(canonicalId, PdaRankingStatId::HelpWounded) == 0)
	{
		return xr_strcmp(statId.c_str(), "help_stalkers") == 0;
	}

	if (xr_strcmp(canonicalId, PdaRankingStatId::Deaths) == 0)
	{
		return xr_strcmp(statId.c_str(), "death") == 0
			|| xr_strcmp(statId.c_str(), "player_deaths") == 0
			|| xr_strcmp(statId.c_str(), "actor_deaths") == 0
			|| xr_strcmp(statId.c_str(), "pda_stat_11") == 0;
	}

	if (xr_strcmp(canonicalId, PdaRankingStatId::Distance) == 0)
	{
		return xr_strcmp(statId.c_str(), "distance_km") == 0
			|| xr_strcmp(statId.c_str(), "km") == 0
			|| xr_strcmp(statId.c_str(), "traveled_km") == 0
			|| xr_strcmp(statId.c_str(), "player_distance") == 0
			|| xr_strcmp(statId.c_str(), "pda_stat_12") == 0;
	}

	return false;
}

bool TryFormatActorStatByIndex(CActor* actor, const u32 index, string64& buffer)
{
	if (!actor)
	{
		return false;
	}

	switch (index)
	{
	case rankingStatActorMoneyEarnedIndex:
		xr_sprintf(buffer, sizeof(buffer), "%u", actor->GetStatMoneyEarned());
		return true;
	case rankingStatActorMoneySpentIndex:
		xr_sprintf(buffer, sizeof(buffer), "%u", actor->GetStatMoneySpent());
		return true;
	case rankingStatActorHelpWoundedIndex:
		xr_sprintf(buffer, sizeof(buffer), "%u", actor->GetStatHelpWounded());
		return true;
	case rankingStatActorHeadshotsIndex:
		xr_sprintf(buffer, sizeof(buffer), "%u", actor->GetStatHeadshots());
		return true;
	case rankingStatActorDeathsIndex:
		xr_sprintf(buffer, sizeof(buffer), "%u", actor->GetStatDeaths());
		return true;
	case rankingStatActorDistanceIndex:
		xr_sprintf(buffer, sizeof(buffer), "%.2f km", actor->GetStatDistanceMeters() / 1000.0f);
		return true;
	default:
		return false;
	}
}

bool TryFormatActorStatById(CActor* actor, const shared_str& statId, string64& buffer)
{
	if (!actor || statId.size() == 0)
	{
		return false;
	}

	if (RankingStatIdMatches(statId, PdaRankingStatId::MoneyEarned))
	{
		xr_sprintf(buffer, sizeof(buffer), "%u", actor->GetStatMoneyEarned());
		return true;
	}
	if (RankingStatIdMatches(statId, PdaRankingStatId::MoneySpent))
	{
		xr_sprintf(buffer, sizeof(buffer), "%u", actor->GetStatMoneySpent());
		return true;
	}
	if (RankingStatIdMatches(statId, PdaRankingStatId::HelpWounded))
	{
		xr_sprintf(buffer, sizeof(buffer), "%u", actor->GetStatHelpWounded());
		return true;
	}
	if (RankingStatIdMatches(statId, PdaRankingStatId::Headshots))
	{
		xr_sprintf(buffer, sizeof(buffer), "%u", actor->GetStatHeadshots());
		return true;
	}
	if (RankingStatIdMatches(statId, PdaRankingStatId::Deaths))
	{
		xr_sprintf(buffer, sizeof(buffer), "%u", actor->GetStatDeaths());
		return true;
	}
	if (RankingStatIdMatches(statId, PdaRankingStatId::Distance))
	{
		xr_sprintf(buffer, sizeof(buffer), "%.2f km", actor->GetStatDistanceMeters() / 1000.0f);
		return true;
	}

	return false;
}
} // namespace

CUIRankingWnd::CUIRankingWnd()
{
	m_actor_ch_info				= nullptr;
	m_previous_time				= Device.dwTimeGlobal;
	m_statPreviousTime			= Device.dwTimeGlobal;
	m_actorStatRevision			= 0;
	m_delay						= 3000;
	m_statDelay					= 250;
	m_last_monster_icon_back	= "";
	m_last_monster_icon			= "";
	m_last_weapon_icon			= "";
	LoadCallbackGlobals(m_isGetRankingsArraySize, m_onGetRankingsArraySize, PdaScript::OnGetRankingsArraySize);
	LoadCallbackGlobals(m_isGetPdaStatById, m_onGetPdaStatById, PdaScript::OnGetPdaStatById);
}

CUIRankingWnd::~CUIRankingWnd()
{
	ACHIEVES_VEC_IT b = m_achieves_vec.begin(), e = m_achieves_vec.end();
	for(; b!=e; b++)
		xr_delete(*b);
	m_achieves_vec.clear();

	if (m_coc_ranking_actor)
	{
		//Alundaio: CoC Rankings
		RANKINGCOC_VEC_IT be = m_coc_ranking_vec.begin(), en = m_coc_ranking_vec.end();
		for (; be != en; be++)
			xr_delete(*be);
		m_coc_ranking_vec.clear();

		xr_delete(m_coc_ranking_actor);
		//-Alundaio
	}
}

void CUIRankingWnd::Show( bool status )
{
	if (status && Actor())
	{
		if (m_actor_ch_info)
		{
			m_actor_ch_info->InitCharacter(Actor());
		}
		if (m_ranking_actor_identity)
		{
			m_ranking_actor_identity->InitCharacter(Actor());
		}

		if (m_money_value)
		{
			string64 buf;
			xr_sprintf(buf, sizeof(buf), "%d %s", Actor()->get_money(), "RU");
			m_money_value->SetText(buf);
			m_money_value->AdjustWidthToText();
		}

		m_actorStatRevision = Actor()->GetPdaRankingStatRevision();
		m_statPreviousTime = Device.dwTimeGlobal;
		m_previous_time = Device.dwTimeGlobal;
		RefreshStatItems();
		update_ranking_heavy();
		inherited::Update();
	}
	inherited::Show( status );
}

void CUIRankingWnd::Update()
{
	inherited::Update();
	if (!IsShown())
	{
		return;
	}

	RefreshStatItemsIfNeeded();

	if (Device.dwTimeGlobal - m_previous_time > m_delay)
	{
		m_previous_time = Device.dwTimeGlobal;
		update_ranking_heavy();
	}
}

void CUIRankingWnd::Init()
{
	Fvector2 pos;
	CUIXml xml;
	xml.Load( CONFIG_PATH, UI_PATH, PdaXml::Ranking );

	CUIXmlInit::InitWindow( xml, "main_wnd", 0, this );
	XML_NODE* stored_root = xml.GetLocalRoot();
	m_delay				= (u32)xml.ReadAttribInt( "main_wnd", 0, "delay",	3000 );
	m_statDelay			= (u32)xml.ReadAttribInt( "main_wnd", 0, "stat_delay", 250 );

    m_background = UIHelper::CreateFrameWindow(xml, "background", this, false);
    if (!m_background)
        m_background2 = UIHelper::CreateFrameLine(xml, "background", this, false);

	if (xml.NavigateToNode("center_background"))
		m_center_background = UIHelper::CreateStatic(xml, "center_background", this);

    m_down_background = UIHelper::CreateFrameWindow(xml, "down_background", this, false);

	const static bool isCharacterInfo = EngineExternal()[EEngineExternalUI::DisableCharacterInfo];
	if (!isCharacterInfo)
	{
		if (xml.NavigateToNode("actor_ch_info", 0))
		{
			m_actor_ch_info = new CUICharacterInfo();
			m_actor_ch_info->SetAutoDelete(true);
			AttachChild(m_actor_ch_info);
			m_actor_ch_info->InitCharacterInfo(&xml, "actor_ch_info");
		}
		if (xml.NavigateToNode("actor_icon_over", 0))
			m_icon_overlay = UIHelper::CreateFrameWindow(xml, "actor_icon_over", this, false);
		if (xml.NavigateToNode("money_caption", 0))
			m_money_caption = UIHelper::CreateStatic(xml, "money_caption", this);
		if (xml.NavigateToNode("money_value", 0))
			m_money_value = UIHelper::CreateStatic(xml, "money_value", this);

		if (m_money_caption)
		{
			m_money_caption->AdjustWidthToText();
			pos = m_money_caption->GetWndPos();
			pos.x += m_money_caption->GetWndSize().x + 10.0f;
		}
		if (m_money_value)
			m_money_value->SetWndPos(pos);

		if (xml.NavigateToNode("center_caption", 0))
			m_center_caption = UIHelper::CreateStatic(xml, "center_caption", this);

		if (xml.NavigateToNode("fraction_static"))
			m_faction_static = UIHelper::CreateStatic(xml, "fraction_static", this);

		m_faction_line1 = UIHelper::CreateFrameLine(xml, "fraction_line1", this, false);
		m_faction_line2 = UIHelper::CreateFrameLine(xml, "fraction_line2", this, false);

	}

	InitStatInfo(xml);

	if (m_center_caption)
	{
		string256 buf;
		xr_strcpy(buf, sizeof(buf), m_center_caption->GetText());
		xr_strcat(buf, sizeof(buf), g_pStringTable->translate("ui_ranking_center_caption").c_str());
		m_center_caption->SetText(buf);
	}

	if (xml.NavigateToNode("fraction_list"))
	{
		m_factions_list = new CUIScrollView();
		CUIXmlInit::InitScrollView(xml, "fraction_list", 0, m_factions_list);
		m_factions_list->SetAutoDelete(true);
		AttachChild(m_factions_list);
		m_factions_list->SetWindowName("---fraction_list");
		m_factions_list->m_sort_function = fastdelegate::MakeDelegate(this, &CUIRankingWnd::SortingLessFunction);

		const char* fract_section = "pda_rank_communities";

		if (pSettings->section_exist(fract_section))
		{
			XML_NODE* node = xml.NavigateToNode("fraction_list", 0);
			xml.SetLocalRoot(node);
			CInifile::Sect& faction_section = pSettings->r_section(fract_section);
			for (const auto& item : faction_section.Data)
			{
				add_faction(xml, item.first);
			}
			xml.SetLocalRoot(stored_root);
		}
	}

	if (xml.NavigateToNode("monster_icon_back"))
		m_monster_icon_back		= UIHelper::CreateStatic(xml, "monster_icon_back", this);

	if (xml.NavigateToNode("monster_icon"))
		m_monster_icon			= UIHelper::CreateStatic(xml, "monster_icon", this);
	if (xml.NavigateToNode("monster_background"))
		m_monster_background	= UIHelper::CreateFrameWindow(xml, "monster_background", this);
	if (xml.NavigateToNode("monster_over"))
		m_monster_over			= UIHelper::CreateFrameWindow(xml, "monster_over", this);

	if (xml.NavigateToNode("favorite_weapon_back"))
		m_favorite_weapon_bckgrnd	= UIHelper::CreateStatic(xml, "favorite_weapon_back", this);
	if (xml.NavigateToNode("favorite_weapon_icon"))
		m_favorite_weapon_icon		= UIHelper::CreateStatic(xml, "favorite_weapon_icon", this);
	if (xml.NavigateToNode("favorite_weapon_ramka"))
		m_favorite_weapon_ramka		= UIHelper::CreateFrameWindow(xml, "favorite_weapon_ramka", this);
	if (xml.NavigateToNode("favorite_weapon_over"))
		m_favorite_weapon_over		= UIHelper::CreateFrameWindow(xml, "favorite_weapon_over", this);

	if (xml.NavigateToNode("valuable_artifact_icon", 0))
		m_valuable_artifact_icon = UIHelper::CreateStatic(xml, "valuable_artifact_icon", this);
	if (xml.NavigateToNode("valuable_artifact_back"))
		UIHelper::CreateFrameWindow(xml, "valuable_artifact_back", this);
	if (xml.NavigateToNode("valuable_artifact_over"))
		UIHelper::CreateFrameWindow(xml, "valuable_artifact_over", this);
	if (xml.NavigateToNode("ranking_actor_identity", 0))
	{
		m_ranking_actor_identity = new CUICharacterInfo();
		m_ranking_actor_identity->SetAutoDelete(true);
		AttachChild(m_ranking_actor_identity);
		m_ranking_actor_identity->InitCharacterInfo(&xml, "ranking_actor_identity");
	}

	m_achievements_background	= UIHelper::CreateFrameWindow(xml, "achievements_background", this, false);
	if (xml.NavigateToNode("achievements_wnd"))
	{
		m_achievements = new CUIScrollView();
		CUIXmlInit::InitScrollView(xml, "achievements_wnd", 0, m_achievements);
		m_achievements->SetAutoDelete(true);
		AttachChild(m_achievements);
	}
	if (m_achievements)
	{
		m_achievements->SetWindowName("achievements_list");

		const char* section = "achievements";

		if (pSettings->section_exist(section))
		{
			CInifile::Sect& achievs_section = pSettings->r_section(section);
			for (const auto& item : achievs_section.Data)
				add_achievement(xml, item.first);
		}
	}
	// Alundaio: CoC Rankings
	if (xml.NavigateToNode("coc_ranking_background", 0))
	{
		m_coc_ranking_background = UIHelper::CreateFrameWindow(xml, "coc_ranking_background", this);
	}
	if (xml.NavigateToNode("coc_ranking_wnd", 0))
	{
		m_coc_ranking = new CUIScrollView();
		CUIXmlInit::InitScrollView(xml, "coc_ranking_wnd", 0, m_coc_ranking);
		m_coc_ranking->SetAutoDelete(true);
		AttachChild(m_coc_ranking);
		m_coc_ranking->SetWindowName("coc_ranking_list");
	}

	if (m_isGetRankingsArraySize)
	{
		u8 topRankCount = 50;
		PdaScriptBridge::TryCall(m_onGetRankingsArraySize, topRankCount);

		if (m_coc_ranking != nullptr)
		{
			for (u8 i = 1; i <= topRankCount; i++)
			{
				CUIRankingsCoC* character_rank_item = new CUIRankingsCoC(m_coc_ranking);
				character_rank_item->init_from_xml(xml, i, false);
				m_coc_ranking_vec.push_back(character_rank_item);
			}
		}
		if (xml.NavigateToNode("coc_ranking_wnd_actor", 0))
		{
			m_coc_ranking_actor_view = new CUIScrollView();
			CUIXmlInit::InitScrollView(xml, "coc_ranking_wnd_actor", 0, m_coc_ranking_actor_view);
			m_coc_ranking_actor_view->SetAutoDelete(true);
			AttachChild(m_coc_ranking_actor_view);
			m_coc_ranking_actor_view->SetWindowName("coc_ranking_list_actor");

			m_coc_ranking_actor = new CUIRankingsCoC(m_coc_ranking_actor_view);
			m_coc_ranking_actor->init_from_xml(xml, topRankCount + 1, true);
		}
		//-Alundaio
	}

	xml.SetLocalRoot(stored_root);
	m_gamepad_legend = UIHelper::CreateGamepadLegend(xml, "gamepad_legend", this, false);
}

void CUIRankingWnd::add_faction(CUIXml& xml, shared_str const& faction_id)
{
	CUIRankFaction* faction = new CUIRankFaction(faction_id);
	faction->init_from_xml(xml);
	faction->SetWindowName("fraction_item");
	m_factions_list->AddWindow(faction, true);
	Register(faction);
}

void CUIRankingWnd::clear_all_factions()
{
	m_factions_list->Clear();
}

void CUIRankingWnd::add_achievement(CUIXml& xml, shared_str const& achiev_id)
{
	CUIAchievements* achievement = new CUIAchievements(m_achievements);
	achievement->init_from_xml(xml);

	achievement->SetName(pSettings->r_string(achiev_id, "name"));
	achievement->SetDescription(pSettings->r_string(achiev_id, "desc"));
	achievement->SetHint(pSettings->r_string(achiev_id, "hint"));
	achievement->SetIcon(pSettings->r_string(achiev_id, "icon"));
	achievement->SetFunctor(pSettings->r_string(achiev_id, "functor"));
	achievement->SetRepeatable(pSettings->read_if_exists<bool>(achiev_id,"repeatable",false));

	m_achieves_vec.push_back(achievement);
}

void CUIRankingWnd::update_info()
{
	update_ranking_heavy();
	RefreshStatItems();
}

void CUIRankingWnd::update_ranking_heavy()
{
	for (const auto& achievement : m_achieves_vec)
		achievement->Update();

	if (m_coc_ranking_actor)
	{
		//Alundaio: CoC Ranking
		RANKINGCOC_VEC_IT begin = m_coc_ranking_vec.begin(), end = m_coc_ranking_vec.end();
		for (; begin != end; begin++)
			(*begin)->Update();

		m_coc_ranking_actor->Update();
		//-Alundaio
	}
	get_best_monster();
	get_favorite_weapon();
	get_valuable_artifact_icon();
	
    if (!m_factions_list)
        return;

    bool force_rating = false;
    for (u8 i = 0; i < m_factions_list->GetSize(); ++i)
    {
        CUIRankFaction* ui_faction = smart_cast<CUIRankFaction*>(m_factions_list->GetItem(i));
        if (ui_faction)
        {
            if (ui_faction->get_cur_sn() != i + 1)
            {
                force_rating = true;
                break;
            }
        }
    }

    for (u8 i = 0; i < m_factions_list->GetSize(); ++i)
    {
        CUIRankFaction* ui_faction = smart_cast<CUIRankFaction*>(m_factions_list->GetItem(i));
        if (ui_faction)
        {
            ui_faction->update_info(i + 1);
            ui_faction->rating(i + 1, force_rating);
        }
    }

    m_factions_list->ForceUpdate();
}

void CUIRankingWnd::RefreshStatItemsIfNeeded()
{
	CActor* actor = Actor();
	if (!actor)
	{
		return;
	}

	const u32 revision = actor->GetPdaRankingStatRevision();
	const bool revisionChanged = revision != m_actorStatRevision;
	const bool statIntervalElapsed = m_statDelay == 0
		|| Device.dwTimeGlobal - m_statPreviousTime >= m_statDelay;

	if (!revisionChanged && !statIntervalElapsed)
	{
		return;
	}

	if (revisionChanged)
	{
		m_actorStatRevision = revision;
	}

	if (statIntervalElapsed)
	{
		m_statPreviousTime = Device.dwTimeGlobal;
	}

	RefreshStatItems();
}

void CUIRankingWnd::InitStatInfo(CUIXml& xml)
{
	XML_NODE* storedRoot = xml.GetLocalRoot();
	XML_NODE* statInfoNode = xml.NavigateToNode("stat_info", 0);
	if (!statInfoNode)
	{
		return;
	}

	xml.SetLocalRoot(statInfoNode);

	const u32 valueColor = CUIXmlInit::GetColor(xml, "value", 0, 0xFFffffff);
	const u32 legacyCount = (u32)xml.GetNodesNum(statInfoNode, "stat");
	const u32 rowCount = (u32)xml.GetNodesNum(statInfoNode, "stat_row");

	const bool hasStatColumns = xml.NavigateToNode(statInfoNode, "stat_columns", 0) != nullptr;
	const char* captionsStackPath = hasStatColumns ? "stat_columns:stat_captions_stack" : "stat_captions_stack";
	const char* valuesStackPath = hasStatColumns ? "stat_columns:stat_values_stack" : "stat_values_stack";

	XML_NODE* captionsStackNode = xml.NavigateToNode(statInfoNode, captionsStackPath, 0);
	XML_NODE* valuesStackNode = xml.NavigateToNode(statInfoNode, valuesStackPath, 0);
	const bool hasCaptionsStack = captionsStackNode != nullptr;
	const bool hasValuesStack = valuesStackNode != nullptr;

	u32 splitCount = 0;
	if (hasCaptionsStack && hasValuesStack)
	{
		u32 captionCount = (u32)xml.GetNodesNum(captionsStackNode, "stat_caption");
		u32 valueCount = (u32)xml.GetNodesNum(valuesStackNode, "stat_value");
		if (captionCount == 0)
		{
			captionCount = (u32)xml.GetNodesNum(statInfoNode, "stat_caption");
		}
		if (valueCount == 0)
		{
			valueCount = (u32)xml.GetNodesNum(statInfoNode, "stat_value");
		}
		if (captionCount == valueCount)
		{
			splitCount = captionCount;
		}
	}
	else if (hasCaptionsStack != hasValuesStack)
	{
		VERIFY2(false, "stat_info: stat_captions_stack and stat_values_stack must both be present");
	}

	m_stat_items.clear();
	m_stat_items.reserve(legacyCount + splitCount + rowCount);

	if (xml.NavigateToNode("stat_list", 0))
	{
		_statList = UIHelper::CreateStackPanel(xml, "stat_list", this, false);
		if (_statList)
		{
			_statList->Show(true);
		}
	}

	for (u32 i = 0; i < legacyCount; ++i)
	{
		InitLegacyStat(xml, statInfoNode, i, valueColor);
	}

	if (hasCaptionsStack && hasValuesStack)
	{
		InitSplitStatColumns(xml, statInfoNode, valueColor);
	}

	for (u32 i = 0; i < rowCount; ++i)
	{
		InitStackedStatRow(xml, statInfoNode, i, valueColor);
	}

	m_stat_count = (u32)m_stat_items.size();
	xml.SetLocalRoot(storedRoot);
}

bool CUIRankingWnd::InitLegacyStat(CUIXml& xml, XML_NODE* statInfoNode, const u32 index, const u32 valueColor)
{
	Fvector2 pos;
	StatItem item = {};
	item.layout = StatItem::ELayout::Legacy;
	item.caption = new CUIStatic();
	AttachChild(item.caption);
	item.caption->SetAutoDelete(true);

	if (!CUIXmlInit::InitStatic(xml, "stat", (int)index, item.caption))
	{
		xr_delete(item.caption);
		return false;
	}

	item.caption->AdjustWidthToText();

	item.value = new CUIStatic();
	AttachChild(item.value);
	item.value->SetAutoDelete(true);
	CUIXmlInit::InitStatic(xml, "stat", (int)index, item.value);

	item.value->SetTextColor(valueColor);

	pos.y = item.caption->GetWndPos().y;
	pos.x = item.caption->GetWndPos().x + item.caption->GetWndSize().x + 5.0f;
	item.value->SetWndPos(pos);

	XML_NODE* statNode = xml.NavigateToNode(statInfoNode, "stat", index);
	if (statNode)
	{
		item.statId = xml.ReadAttrib(statNode, "id", "");
	}

	m_stat_items.push_back(item);
	return true;
}

bool CUIRankingWnd::InitSplitStatColumns(CUIXml& xml, XML_NODE* statInfoNode, const u32 valueColor)
{
	const bool hasStatColumns = xml.NavigateToNode(statInfoNode, "stat_columns", 0) != nullptr;
	const char* captionsStackPath = hasStatColumns ? "stat_columns:stat_captions_stack" : "stat_captions_stack";
	const char* valuesStackPath = hasStatColumns ? "stat_columns:stat_values_stack" : "stat_values_stack";

	XML_NODE* captionsStackNode = xml.NavigateToNode(statInfoNode, captionsStackPath, 0);
	XML_NODE* valuesStackNode = xml.NavigateToNode(statInfoNode, valuesStackPath, 0);
	if (!captionsStackNode || !valuesStackNode)
	{
		return false;
	}

	u32 captionCount = (u32)xml.GetNodesNum(captionsStackNode, "stat_caption");
	u32 valueCount = (u32)xml.GetNodesNum(valuesStackNode, "stat_value");
	const bool flatCaptions = captionCount == 0;
	const bool flatValues = valueCount == 0;
	if (flatCaptions)
	{
		captionCount = (u32)xml.GetNodesNum(statInfoNode, "stat_caption");
	}
	if (flatValues)
	{
		valueCount = (u32)xml.GetNodesNum(statInfoNode, "stat_value");
	}
	if (captionCount != valueCount)
	{
		VERIFY2(false, make_string<const char*>(
			"stat_info: stat_caption count (%u) != stat_value count (%u)",
			captionCount,
			valueCount));
		return false;
	}

	if (captionCount == 0)
	{
		return true;
	}

	XML_NODE* const captionInitRoot = flatCaptions ? statInfoNode : captionsStackNode;
	XML_NODE* const valueInitRoot = flatValues ? statInfoNode : valuesStackNode;

	CUIWindow* layoutParent = this;
	if (hasStatColumns)
	{
		_statColumns = new CUIWindow();
		_statColumns->SetAutoDelete(true);
		AttachChild(_statColumns);
		if (!CUIXmlInit::InitWindow(xml, "stat_columns", 0, _statColumns, false))
		{
			return false;
		}
		layoutParent = _statColumns;
	}

	xml.SetLocalRoot(statInfoNode);

	_statCaptionsStack = UIHelper::CreateStackPanel(xml, captionsStackPath, layoutParent, false);
	_statValuesStack = UIHelper::CreateStackPanel(xml, valuesStackPath, layoutParent, false);
	if (!_statCaptionsStack || !_statValuesStack)
	{
		return false;
	}

	_statCaptionsStack->Show(true);
	_statValuesStack->Show(true);

	for (u32 i = 0; i < captionCount; ++i)
	{
		StatItem item = {};
		item.layout = StatItem::ELayout::SplitColumns;

		xml.SetLocalRoot(captionInitRoot);
		item.caption = UIHelper::CreateStatic(xml, "stat_caption", _statCaptionsStack, false, (int)i);
		if (!item.caption)
		{
			VERIFY2(false, make_string<const char*>("stat_caption[%u]: init failed", i));
			xml.SetLocalRoot(statInfoNode);
			return false;
		}

		const float captionWidth = xml.ReadAttribFlt("stat_caption", (int)i, "width", 0.f);
		if (captionWidth <= 0.f)
		{
			item.caption->AdjustWidthToText();
		}
		item.caption->Show(true);

		XML_NODE* captionNode = xml.NavigateToNode(captionInitRoot, "stat_caption", i);
		if (captionNode)
		{
			item.statId = xml.ReadAttrib(captionNode, "id", "");
		}

		xml.SetLocalRoot(valueInitRoot);
		item.value = UIHelper::CreateStatic(xml, "stat_value", _statValuesStack, false, (int)i);
		if (!item.value)
		{
			VERIFY2(false, make_string<const char*>("stat_value[%u]: init failed", i));
			xml.SetLocalRoot(statInfoNode);
			return false;
		}
		item.value->SetTextColor(valueColor);
		item.value->Show(true);

		m_stat_items.push_back(item);
	}

	xml.SetLocalRoot(statInfoNode);
	return true;
}

bool CUIRankingWnd::InitStackedStatRow(CUIXml& xml, XML_NODE* statInfoNode, const u32 index, const u32 valueColor)
{
	XML_NODE* statRowNode = xml.NavigateToNode(statInfoNode, "stat_row", index);
	if (!statRowNode)
	{
		return false;
	}

	xml.SetLocalRoot(statRowNode);
	if (!xml.NavigateToNode("stack_panel", 0))
	{
		VERIFY2(false, make_string<const char*>("stat_row[%u]: missing stack_panel", index));
		xml.SetLocalRoot(statInfoNode);
		return false;
	}

	CUIWindow* rowParent = this;
	if (_statList)
	{
		rowParent = _statList;
	}
	CUIWindow* stackParent = rowParent;

	StatItem item = {};
	item.layout = StatItem::ELayout::StackedRow;

	if (!_statList)
	{
		item.rowRoot = new CUIWindow();
		item.rowRoot->SetAutoDelete(true);
		rowParent->AttachChild(item.rowRoot);

		const float rowX = xml.ReadAttribFlt(statRowNode, "x", 0.f);
		const float rowY = xml.ReadAttribFlt(statRowNode, "y", 0.f);
		const float rowW = xml.ReadAttribFlt(statRowNode, "width", 0.f);
		const float rowH = xml.ReadAttribFlt(statRowNode, "height", 0.f);
		item.rowRoot->SetWndPos(Fvector2().set(rowX, rowY));
		if (rowW > 0.f && rowH > 0.f)
		{
			item.rowRoot->SetWndSize(Fvector2().set(rowW, rowH));
		}

		stackParent = item.rowRoot;
	}

	item.rowStack = UIHelper::CreateStackPanel(xml, "stack_panel", stackParent, false);
	if (!item.rowStack)
	{
		xml.SetLocalRoot(statInfoNode);
		return false;
	}
	item.rowStack->Show(true);

	item.caption = new CUIStatic();
	item.caption->SetAutoDelete(true);
	if (!CUIXmlInit::InitStatic(xml, "stat_caption", 0, item.caption))
	{
		VERIFY2(false, make_string<const char*>("stat_row[%u]: failed to init stat_caption", index));
		xml.SetLocalRoot(statInfoNode);
		return false;
	}
	item.caption->AdjustWidthToText();
	item.caption->Show(true);
	item.rowStack->AttachChild(item.caption);

	item.value = new CUIStatic();
	item.value->SetAutoDelete(true);
	if (!CUIXmlInit::InitStatic(xml, "stat_value", 0, item.value))
	{
		VERIFY2(false, make_string<const char*>("stat_row[%u]: failed to init stat_value", index));
		xml.SetLocalRoot(statInfoNode);
		return false;
	}
	item.value->SetTextColor(valueColor);
	item.value->Show(true);
	item.rowStack->AttachChild(item.value);

	item.statId = xml.ReadAttrib(statRowNode, "id", "");

	m_stat_items.push_back(item);
	xml.SetLocalRoot(statInfoNode);
	return true;
}

void CUIRankingWnd::DrawHint()
{
	ACHIEVES_VEC_IT b = m_achieves_vec.begin(), e = m_achieves_vec.end();
	for(; b!=e; b++)
	{
		if((*b)->IsShown())
			(*b)->DrawHint();
	}

	//Alundaio: CoC Ranking
	if (m_coc_ranking_actor)
	{
		RANKINGCOC_VEC_IT begin = m_coc_ranking_vec.begin(), end = m_coc_ranking_vec.end();
		for (; begin != end; begin++)
		{
			if ((*begin)->IsShown())
				(*begin)->DrawHint();
		}

		if (m_coc_ranking_actor->IsShown())
			m_coc_ranking_actor->DrawHint();
	}
	//-Alundaio
}

void CUIRankingWnd::RefreshStatItems()
{
	if (m_stat_items.empty())
	{
		return;
	}

	string128 timeBuf;
	InventoryUtilities::GetTimePeriodAsString(timeBuf, sizeof(timeBuf), Level().GetStartGameTime(), Level().GetGameTime());
	if (m_stat_items[0].value)
	{
		const shared_str timeText = timeBuf;
		if (m_stat_items[0].cachedValue != timeText)
		{
			m_stat_items[0].cachedValue = timeText;
			m_stat_items[0].value->SetText(timeBuf);
		}
	}

	for (u32 i = 1; i < m_stat_count; ++i)
	{
		StatItem& item = m_stat_items[i];
		if (!item.value)
		{
			continue;
		}

		item.value->TextItemControl()->SetColoringMode(true);
		const char* statValue = GetStatValue(item, i);
		if (!statValue || !statValue[0])
		{
			if (item.cachedValue.size() != 0)
			{
				item.cachedValue = "";
				item.value->SetText("");
			}
			continue;
		}

		if (item.cachedValue == statValue)
		{
			continue;
		}

		item.cachedValue = statValue;
		item.value->SetText(statValue);
	}
}

void CUIRankingWnd::get_best_monster()
{
	const char* str = nullptr;

	if (!PdaScriptBridge::TryCall(PdaScript::GetMonsterBack, str) || !str || !str[0])
	{
		return;
	}

	if (xr_strcmp(str, m_last_monster_icon_back))
	{
		if (m_monster_icon_back)
		{
			m_monster_icon_back->TextureOn();
			m_monster_icon_back->InitTexture(str);
		}
		m_last_monster_icon_back = str;
	}

	if (!PdaScriptBridge::TryCall(PdaScript::GetMonsterIcon, str) || !str || !str[0])
	{
		return;
	}

	if (xr_strcmp(str, m_last_monster_icon))
	{
		if (m_monster_icon)
		{
			m_monster_icon->TextureOn();
			m_monster_icon->InitTexture(str);
		}
		m_last_monster_icon = str;
	}
}

void CUIRankingWnd::get_favorite_weapon()
{
	const char* str = nullptr;
	if (!PdaScriptBridge::TryCall(PdaScript::GetFavoriteWeapon, str) || !str || !str[0])
	{
		return;
	}

	if (m_favorite_weapon_icon && xr_strcmp(str, m_last_weapon_icon))
	{
		if(pSettings->section_exist(str) && pSettings->line_exist(str, "upgr_icon_x"))
		{
			const char* upgrIconsTexture = pSettings->read_if_exists<LPCSTR>(str,"upgr_icons_texture",nullptr);
			m_favorite_weapon_icon->SetShader(InventoryUtilities::GetWeaponUpgradeIconsShader(upgrIconsTexture));
			if(!xr_strcmp(str, "wpn_rpg7"))
				m_favorite_weapon_icon->SetShader(InventoryUtilities::GetOutfitUpgradeIconsShader(upgrIconsTexture));

			Frect				tex_rect;
			tex_rect.x1			= float(pSettings->r_u32(str, "upgr_icon_x"));
			tex_rect.y1			= float(pSettings->r_u32(str, "upgr_icon_y"));
			tex_rect.x2			= float(pSettings->r_u32(str, "upgr_icon_width"));
			tex_rect.y2			= float(pSettings->r_u32(str, "upgr_icon_height"));
			tex_rect.rb.add		(tex_rect.lt);
			m_favorite_weapon_icon->SetTextureRect(tex_rect);
			m_favorite_weapon_icon->TextureOn();
			m_favorite_weapon_icon->SetTextureColor(color_rgba(255,255,255,255));
			m_favorite_weapon_icon->SetWndSize(Fvector2().set((tex_rect.x2-tex_rect.x1)*CHudPdaAnimator::GetPDAScreen_kx()*0.8, (tex_rect.y2-tex_rect.y1)*0.8));
			m_favorite_weapon_icon->SetStretchTexture(true);
		}
		m_last_weapon_icon = str;
	}
}

void CUIRankingWnd::get_valuable_artifact_icon()
{
	if (!m_valuable_artifact_icon)
	{
		return;
	}

	const char* str = nullptr;
	if (!PdaScriptBridge::TryCall(PdaScript::GetValuableArtifactIcon, str))
	{
		return;
	}
	if (!str || !xr_strcmp(str, ""))
	{
		m_valuable_artifact_icon->TextureOff();
		m_last_valuable_artifact_icon = shared_str();
		return;
	}

	if (m_last_valuable_artifact_icon != str)
	{
		m_valuable_artifact_icon->TextureOn();
		m_valuable_artifact_icon->InitTexture(str);
		m_last_valuable_artifact_icon = str;
	}
}

bool CUIRankingWnd::SortingLessFunction(CUIWindow* left, CUIWindow* right)
{
	CUIRankFaction* lpi = smart_cast<CUIRankFaction*>(left);
	CUIRankFaction* rpi = smart_cast<CUIRankFaction*>(right);
	VERIFY(lpi && rpi);
	return (lpi->get_faction_power() > rpi->get_faction_power());
}

const char* CUIRankingWnd::GetStatValue(const StatItem& item, const u32 index) const
{
	static string64 actorStatBuffer = {};
	CActor* actor = Actor();

	if (item.statId.size() != 0)
	{
		const bool preferLuaDistanceFormat = m_isGetPdaStatById
			&& RankingStatIdMatches(item.statId, PdaRankingStatId::Distance);
		if (!preferLuaDistanceFormat && TryFormatActorStatById(actor, item.statId, actorStatBuffer))
		{
			return actorStatBuffer;
		}

		if (m_isGetPdaStatById)
		{
			const char* value = nullptr;
			if (PdaScriptBridge::TryCall(m_onGetPdaStatById, item.statId.c_str(), value) && value && value[0])
			{
				return value;
			}
		}

		const char* value = nullptr;
		if (PdaScriptBridge::TryCall(PdaScript::GetStatById, item.statId.c_str(), value) && value && value[0])
		{
			return value;
		}
	}

	const char* indexValue = nullptr;
	if (PdaScriptBridge::TryCall(PdaScript::GetStat, index, indexValue) && indexValue && indexValue[0])
	{
		return indexValue;
	}

	if (TryFormatActorStatByIndex(actor, index, actorStatBuffer))
	{
		return actorStatBuffer;
	}

	return "";
}

void CUIRankingWnd::ResetAll()
{
	m_last_monster_icon_back	= "";
	m_last_monster_icon			= "";
	m_last_weapon_icon			= "";
	m_last_valuable_artifact_icon = shared_str();
	if (m_valuable_artifact_icon)
		m_valuable_artifact_icon->TextureOff();
	if (m_monster_icon_back)
		m_monster_icon_back->TextureOff();
	if (m_monster_icon)
		m_monster_icon->TextureOff();
	if (m_favorite_weapon_icon)
		m_favorite_weapon_icon->TextureOff();
	ACHIEVES_VEC_IT b = m_achieves_vec.begin(), e = m_achieves_vec.end();
	for(; b!=e; b++)
		(*b)->Reset();

	if (m_coc_ranking_actor)
	{
		//Alundaio: CoC Rankings
		RANKINGCOC_VEC_IT be = m_coc_ranking_vec.begin(), ed = m_coc_ranking_vec.end();
		for (; be != ed; be++)
			(*be)->Reset();

		m_coc_ranking_actor->Reset();
	}
	//-Alundaio 

	inherited::ResetAll();
}

#define RANKING_WND_SCROLL_STEP_SIZE 16.0f

static void RankingWndScrollByStep(CUIScrollView* scroll, bool scrollUp)
{
	if (!scroll)
	{
		return;
	}
	CUIScrollBar* bar = scroll->ScrollBar();
	if (!bar)
	{
		return;
	}

	const int orig = bar->GetStepSize();
	bar->SetStepSize(RANKING_WND_SCROLL_STEP_SIZE);
	if (scrollUp)
	{
		bar->TryScrollDec();
	}
	else
	{
		bar->TryScrollInc();
	}
	bar->SetStepSize(orig);
}

bool CUIRankingWnd::OnGamepadKeyAction(int key, EUIMessages gamepad_action)
{
	if (gamepad_action == WINDOW_KEY_PRESSED)
	{
		switch (get_binded_action(key, agUILogMenu))
		{
			case kPDA_LOG_SCROLL_UP:
			{
				if (m_pUiSounds)
				{
					m_pUiSounds->Play(EPdaUiSound::ListScroll, true);
				}
				RankingWndScrollByStep(m_achievements ? m_achievements : m_factions_list, true);
				return true;
			}
			case kPDA_LOG_SCROLL_DOWN:
			{
				if (m_pUiSounds)
				{
					m_pUiSounds->Play(EPdaUiSound::ListScroll, true);
				}
				RankingWndScrollByStep(m_achievements ? m_achievements : m_factions_list, false);
				return true;
			}
		}
	}
	return inherited::OnGamepadKeyAction(key, gamepad_action);
}

bool CUIRankingWnd::OnGamepadKeyHold(int key)
{
	switch (get_binded_action(key, agUILogMenu))
	{
		case kPDA_LOG_SCROLL_UP:
		{
			if (!any_binded_key_for_action_pressed_c(kPDA_LOG_SCROLL_DOWN))
			{
				RankingWndScrollByStep(m_achievements ? m_achievements : m_factions_list, true);
			}
			return true;
		}
		case kPDA_LOG_SCROLL_DOWN:
		{
			if (!any_binded_key_for_action_pressed_c(kPDA_LOG_SCROLL_UP))
			{
				RankingWndScrollByStep(m_achievements ? m_achievements : m_factions_list, false);
			}
			return true;
		}
	}
	return inherited::OnGamepadKeyHold(key);
}
