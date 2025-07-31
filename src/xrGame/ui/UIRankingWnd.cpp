////////////////////////////////////////////////////////////////////////////
//	Module 		: UIRankingWnd.cpp
//	Created 	: 17.01.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Ranking window class implementation
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "UIRankingWnd.h"

#include "../../xrUI/Widgets/UIFixedScrollBar.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/UIHelper.h"
#include "UIInventoryUtilities.h"

#include "../Actor.h"
#include "../ai_space.h"
#include "../alife_simulator.h"

#include "../../xrScripts/script_engine.h"
#include "../character_community.h"
#include "../character_reputation.h"
#include "../relation_registry.h"
#include "../../xrEngine/string_table.h"
#include "UICharacterInfo.h"
#include "../../xrUI/ui_base.h"

#define  PDA_RANKING_XML		"pda_ranking.xml"

using namespace luabind;

CUIRankingWnd::CUIRankingWnd()
{
	m_actor_ch_info				= nullptr;
	m_previous_time				= Device.dwTimeGlobal;
	m_delay						= 3000;
	m_last_monster_icon_back	= "";
	m_last_monster_icon			= "";
	m_last_weapon_icon			= "";
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

		if (m_money_value)
		{
			string64 buf;
			xr_sprintf(buf, sizeof(buf), "%d %s", Actor()->get_money(), "RU");
			m_money_value->SetText(buf);
			m_money_value->AdjustWidthToText();
			update_info();
		}
	}
	inherited::Show( status );
}

void CUIRankingWnd::Update()
{
	if ( Device.dwTimeGlobal - m_previous_time > m_delay )
	{
		m_previous_time = Device.dwTimeGlobal;
		update_info();
	}
}

void CUIRankingWnd::Init()
{
	Fvector2 pos;
	CUIXml xml;
	xml.Load( CONFIG_PATH, UI_PATH, PDA_RANKING_XML );

	CUIXmlInit::InitWindow( xml, "main_wnd", 0, this );
	m_delay				= (u32)xml.ReadAttribInt( "main_wnd", 0, "delay",	3000 );

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
			m_money_caption = UIHelper::CreateTextWnd(xml, "money_caption", this);
		if (xml.NavigateToNode("money_value", 0))
			m_money_value = UIHelper::CreateTextWnd(xml, "money_value", this);

		if (m_money_caption)
		{
			m_money_caption->AdjustWidthToText();
			pos = m_money_caption->GetWndPos();
			pos.x += m_money_caption->GetWndSize().x + 10.0f;
		}
		if (m_money_value)
			m_money_value->SetWndPos(pos);

		if (xml.NavigateToNode("center_caption", 0))
			m_center_caption = UIHelper::CreateTextWnd(xml, "center_caption", this);

		if (xml.NavigateToNode("fraction_static"))
			m_faction_static = UIHelper::CreateStatic(xml, "fraction_static", this);

		m_faction_line1 = UIHelper::CreateFrameLine(xml, "fraction_line1", this, false);
		m_faction_line2 = UIHelper::CreateFrameLine(xml, "fraction_line2", this, false);

	}
	
	XML_NODE* stored_root = xml.GetLocalRoot();
	XML_NODE* node = xml.NavigateToNode( "stat_info", 0 );
	xml.SetLocalRoot( node );

	m_stat_count = (u32)xml.GetNodesNum( node, "stat" );
	u32 value_color = CUIXmlInit::GetColor( xml, "value", 0, 0xFFffffff );

	for ( u8 i = 0; i < m_stat_count; ++i )
	{
		m_stat_caption[i]		= new CUITextWnd();
		AttachChild				( m_stat_caption[i] );
		m_stat_caption[i]->SetAutoDelete( true );

		if (CUIXmlInit::InitTextWnd(xml, "stat", i, m_stat_caption[i]))
		{
			m_stat_caption[i]->AdjustWidthToText();

			m_stat_info[i] = new CUITextWnd();
			AttachChild(m_stat_info[i]);
			m_stat_info[i]->SetAutoDelete(true);
			CUIXmlInit::InitTextWnd(xml, "stat", i, m_stat_info[i]);

			m_stat_info[i]->SetTextColor(value_color);

			pos.y = m_stat_caption[i]->GetWndPos().y;
			pos.x = m_stat_caption[i]->GetWndPos().x + m_stat_caption[i]->GetWndSize().x + 5.0f;
			m_stat_info[i]->SetWndPos(pos);
		}
	}
	xml.SetLocalRoot( stored_root );

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

		pcstr fract_section = "pda_rank_communities";

		if (pSettings->section_exist(fract_section))
		{
			node = xml.NavigateToNode("fraction_list", 0);
			xml.SetLocalRoot(node);
			CInifile::Sect& faction_section = pSettings->r_section(fract_section);
			for (const auto& item : faction_section.Data)
			{
				add_faction(xml, item.first);
			}
			node = xml.NavigateToNode("fraction_list", 0);
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

		pcstr section = "achievements";

		if (pSettings->section_exist(section))
		{
			CInifile::Sect& achievs_section = pSettings->r_section(section);
			for (const auto& item : achievs_section.Data)
				add_achievement(xml, item.first);
		}
	}
	// Alundaio: CoC Rankings
	// St4lker0k765: it's not necessary for CoP anymore
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

	u8 topRankCount = 50;
	luabind::functor<u8> getRankingArraySize;

	if (ai().script_engine().functor("pda.get_rankings_array_size", getRankingArraySize))
	{
		topRankCount = getRankingArraySize();
	}

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

	xml.SetLocalRoot(stored_root);
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
	achievement->SetRepeatable(!!READ_IF_EXISTS(pSettings,r_bool,achiev_id,"repeatable",false));

	m_achieves_vec.push_back(achievement);
}

void CUIRankingWnd::update_info()
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
	get_statistic();
	get_best_monster();
	get_favorite_weapon();
	
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
    get_value_from_script();
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

void CUIRankingWnd::get_statistic()
{
	/*
	string128 buf;
	InventoryUtilities::GetTimePeriodAsString(buf, sizeof(buf), Level().GetStartGameTime(), Level().GetGameTime());
	m_stat_info[0]->SetTextColor(color_rgba(170,170,170,255));
	m_stat_info[0]->SetText(buf);
	*/

	for(u8 i = 0; i < m_stat_count; ++i)
	{
		luabind::functor<LPCSTR> funct;
		if (ai().script_engine().functor("pda.get_stat", funct))
		{
			LPCSTR str = funct(i);
			// m_stat_info[i]->SetTextColor(color_rgba(170, 170, 170, 255));
			m_stat_info[i]->TextItemControl().SetColoringMode(true);
			m_stat_info[i]->SetTextST(str);
		}
	}

}

void CUIRankingWnd::get_best_monster()
{
	pcstr str;
	luabind::functor<pcstr> functor;

	if (ai().script_engine().functor("pda.get_monster_back", functor))
	{
		str = functor();
		if (!xr_strcmp(str, ""))
			return;

		if (xr_strcmp(str, m_last_monster_icon_back))
		{
			if (m_monster_icon_back)
			{
				m_monster_icon_back->TextureOn();
				m_monster_icon_back->InitTexture(str);
			}
			m_last_monster_icon_back = str;
		}
	}

	if (ai().script_engine().functor("pda.get_monster_icon", functor))
	{
		str = functor();
		if (!xr_strcmp(str, ""))
			return;

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
}

void CUIRankingWnd::get_favorite_weapon()
{
	luabind::functor<pcstr> functor;
	if (!ai().script_engine().functor("pda.get_favorite_weapon", functor))
		return;
	pcstr str = functor();

	if(!xr_strcmp(str, ""))
		return;

	if(m_favorite_weapon_icon && xr_strcmp(str, m_last_weapon_icon))
	{
		if(pSettings->section_exist(str) && pSettings->line_exist(str, "upgr_icon_x"))
		{
			const char* upgrIconsTexture = READ_IF_EXISTS(pSettings, r_string, str, "upgr_icons_texture", nullptr);
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
			m_favorite_weapon_icon->SetWndSize(Fvector2().set((tex_rect.x2-tex_rect.x1)*UI().get_current_kx()*0.8, (tex_rect.y2-tex_rect.y1)*0.8));
			m_favorite_weapon_icon->SetStretchTexture(true);
		}
		m_last_weapon_icon = str;
	}
}

bool CUIRankingWnd::SortingLessFunction(CUIWindow* left, CUIWindow* right)
{
	CUIRankFaction* lpi = smart_cast<CUIRankFaction*>(left);
	CUIRankFaction* rpi = smart_cast<CUIRankFaction*>(right);
	VERIFY(lpi && rpi);
	return (lpi->get_faction_power() > rpi->get_faction_power());
}

void CUIRankingWnd::get_value_from_script()
{
	string128 buf;
	InventoryUtilities::GetTimePeriodAsString(buf, sizeof(buf), Level().GetStartGameTime(), Level().GetGameTime());
	m_stat_info[0]->SetText(buf);

	for (u8 i = 1; i < m_stat_count; ++i)
	{
		luabind::functor<pcstr> functor;
		if (ai().script_engine().functor("pda.get_stat", functor))
		{
			pcstr str = functor(i);
			m_stat_info[i]->SetTextST(str);
		}
	}
}

void CUIRankingWnd::ResetAll()
{
	m_last_monster_icon_back	= "";
	m_last_monster_icon			= "";
	m_last_weapon_icon			= "";
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