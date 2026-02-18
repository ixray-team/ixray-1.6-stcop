#include "StdAfx.h"
#include "UIStalkersRankingWnd.h"

#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "UIPdaListItem.h"
#include "../../xrUI/Widgets/UIAnimatedStatic.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "UICharacterInfo.h"
#include "../InventoryOwner.h"
#include "../Level.h"
#include "../PDA.h"
#include "../Actor.h"
#include "xrServer_Objects_ALife_Monsters.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIHint.h"
#include "../../xrUI/UICursor.h"
#include "../../xrEngine/string_table.h"

#define		STALKERS_RANKING_XML			"stalkers_ranking.xml"
#define		STALKERS_RANKING_CHARACTER_XML	"stalkers_ranking_character.xml"

struct SStatData{
	ALife::_OBJECT_ID							id;
	CSE_ALifeTraderAbstract*	trader;
	bool operator == (const SStatData& d1){return (id==d1.id) ;}
};

typedef xr_vector<SStatData>	TOP_LIST;
TOP_LIST						g_all_statistic_humans;

CUIStalkersRankingWnd::CUIStalkersRankingWnd()
{
	ActionRepeaters()->Register(this, kUI_DOWN);
	ActionRepeaters()->Register(this, kUI_UP);
}

CUIStalkersRankingWnd::~CUIStalkersRankingWnd()
{
	ActionRepeaters()->UnregisterOwner(this);
}

void CUIStalkersRankingWnd::Init()
{
	CUIXml								uiXml;
	uiXml.Load							(CONFIG_PATH, UI_PATH,STALKERS_RANKING_XML);

	CUIXmlInit							xml_init;

	xml_init.InitWindow					(uiXml, "main_wnd", 0, this);

	CUIWindow* frameParent = this;
	if (uiXml.NavigateToNode("background"))
	{
		m_background = UIHelper::CreateFrameWindow(uiXml, "background", this);
		frameParent = m_background;
	}

	UICharIconFrame						= new CUIFrameWindow(); UICharIconFrame->SetAutoDelete(true);
	frameParent->AttachChild			(UICharIconFrame);
	xml_init.InitFrameWindow			(uiXml, "chicon_frame_window", 0, UICharIconFrame);

	UICharIconHeader					= new CUIFrameLineWnd(); UICharIconHeader->SetAutoDelete(true);
	UICharIconFrame->AttachChild		(UICharIconHeader);
	xml_init.InitFrameLine				(uiXml, "chicon_frame_line", 0, UICharIconHeader);


	UIInfoFrame							= new CUIFrameWindow(); UIInfoFrame->SetAutoDelete(true);
	frameParent->AttachChild			(UIInfoFrame);
	xml_init.InitFrameWindow			(uiXml, "info_frame_window", 0, UIInfoFrame);
	
	UIInfoHeader						= new CUIFrameLineWnd(); UIInfoHeader->SetAutoDelete(true);
	UIInfoFrame->AttachChild			(UIInfoHeader);
	xml_init.InitFrameLine				(uiXml, "info_frame_line", 0, UIInfoHeader);

	UIAnimatedIcon						= new CUIAnimatedStatic(); UIAnimatedIcon->SetAutoDelete(true);
	UIInfoHeader->AttachChild			(UIAnimatedIcon);
	xml_init.InitAnimatedStatic			(uiXml, "a_static", 0, UIAnimatedIcon);

	UIList								= new CUIScrollView(); UIList->SetAutoDelete(true);
	UIInfoFrame->AttachChild			(UIList);
	xml_init.InitScrollView				(uiXml, "list", 0, UIList);
	m_items_count						= uiXml.ReadAttribInt("list", 0, "item_count", 20);

	UICharacterWindow					= new CUIWindow(); UICharacterWindow->SetAutoDelete(true);
	UICharIconFrame->AttachChild		(UICharacterWindow);
	xml_init.InitWindow					(uiXml, "character_info", 0, UICharacterWindow);

	UICharacterInfo						= new CUICharacterInfo(); UICharacterInfo->SetAutoDelete(true);
	UICharacterWindow->AttachChild		(UICharacterInfo);
	UICharacterInfo->InitCharacterInfo	(Fvector2().set(0,0),UICharacterWindow->GetWndSize(), STALKERS_RANKING_CHARACTER_XML);

	xml_init.InitAutoStaticGroup		(uiXml, "left_auto",	0,			UIInfoFrame);
	xml_init.InitAutoStaticGroup		(uiXml, "right_auto",	0,			UICharIconFrame);

	if (uiXml.NavigateToNode("hint_wnd"))
	{
		m_hint_wnd = UIHelper::CreateHint(uiXml, "hint_wnd");
	}
	m_gamepad_legend = UIHelper::CreateGamepadLegend(uiXml, "gamepad_legend", this, false);
}

void CUIStalkersRankingWnd::DrawHint()
{
	if (m_hint_wnd)
		m_hint_wnd->Draw();
}

void CUIStalkersRankingWnd::Show(bool status)
{
	inherited::Show(status);
	if (status)
		FillList								();
}

bool GreaterRankPred(const SStatData& h1, const SStatData& h2)
{
	return (h1.trader->m_rank > h2.trader->m_rank);
}

extern CSE_ALifeTraderAbstract* ch_info_get_from_id (ALife::_OBJECT_ID id);

int get_actor_ranking()
{
	std::sort	(g_all_statistic_humans.begin(),g_all_statistic_humans.end(),GreaterRankPred);
	CSE_ALifeTraderAbstract* pActorAbstract = ch_info_get_from_id(Actor()->ID());
	SStatData	d;
	d.id		= Actor()->ID();
	d.trader	= pActorAbstract;

	TOP_LIST::iterator it = std::find(g_all_statistic_humans.begin(),g_all_statistic_humans.end(),d);
	if(it!=g_all_statistic_humans.end())
		return (int)std::distance(g_all_statistic_humans.begin(), it);
	else
		return		1;
}

void CUIStalkersRankingWnd::FillList()
{

	CUIXml									uiXml;
	uiXml.Load								(CONFIG_PATH, UI_PATH,STALKERS_RANKING_XML);

	UIList->Clear							();

	uiXml.SetLocalRoot						(uiXml.NavigateToNode("stalkers_list",0));

	if(g_all_statistic_humans.size())
	{
		CSE_ALifeTraderAbstract* pActorAbstract = ch_info_get_from_id(Actor()->ID());
		int actor_place							= get_actor_ranking();

		int sz = std::min(g_all_statistic_humans.size(),(size_t)m_items_count);
		for (int i = 0; i < sz; ++i)
		{
			CSE_ALifeTraderAbstract* pT			= (g_all_statistic_humans[i]).trader;
			if (!pT || pT->object_id() == ALife::_OBJECT_ID(-1))
			{
				continue;
			}

			if(pT==pActorAbstract || (i== m_items_count-1&&actor_place>m_items_count-1)  )
			{
				AddActorItem					(&uiXml, actor_place+1, pActorAbstract);
			}
			else
			{
				AddStalkerItem					(&uiXml, i+1, pT);
			}
		}

		UIList->SetSelected						(UIList->GetItem(0) );
	}else{
		CUIStalkerRankingInfoItem* itm		= new CUIStalkerRankingInfoItem(this);
		itm->Init							(&uiXml, "no_items", 0);
		UIList->AddWindow					(itm, true);
	}
}

void CUIStalkersRankingWnd::ShowHumanInfo(ALife::_OBJECT_ID id)
{
	UICharacterInfo->InitCharacter(id);
}

void CUIStalkersRankingWnd::AddStalkerItem(CUIXml* xml, int num, CSE_ALifeTraderAbstract* t)
{
	string256							buff;
	CUIStalkerRankingInfoItem* itm		= new CUIStalkerRankingInfoItem(this);
	itm->Init							(xml, "item_human", 0);
	itm->set_hint_wnd					(m_hint_wnd);
	itm->set_hint_delay					(0);

	xr_sprintf								(buff,"%d.",num);
	itm->m_text1->SetText				(buff);		

	xr_sprintf								(buff,"%s",t->m_character_name.c_str());
	itm->m_text2->SetText				(buff);		

	xr_sprintf								(buff,"%d",t->m_rank);
	itm->m_text3->SetText				(buff);		
	itm->m_humanID						= t->object_id();
	UIList->AddWindow					(itm, true);

}

void CUIStalkersRankingWnd::AddActorItem(CUIXml* xml, int num, CSE_ALifeTraderAbstract* t)
{
	string64							buff;
	CUIStalkerRankingInfoItem*			itm;
	if(num > m_items_count-1)
	{
		itm								= new CUIStalkerRankingElipsisItem(this);
		itm->Init						(xml, "item_ellipsis", 0);
		UIList->AddWindow				(itm, true);
	}

	itm									= new CUIStalkerRankingInfoItem(this);
	itm->Init							(xml, "item_actor", 0);
	itm->set_hint_wnd					(m_hint_wnd);
	itm->set_hint_delay					(0);

	xr_sprintf								(buff,"%d.",num);
	itm->m_text1->SetText				(buff);		


	xr_sprintf								(buff,"%s", t->m_character_name.c_str());
	itm->m_text2->SetText				(buff);		

	xr_sprintf								(buff,"%d",t->m_rank);
	itm->m_text3->SetText				(buff);		

	itm->m_humanID						= t->object_id();
	UIList->AddWindow					(itm, true);
}

void CUIStalkersRankingWnd::Reset()
{
	inherited::Reset		();	
}

bool CUIStalkersRankingWnd::OnGamepadKeyAction(int id, EUIMessages gamepad_action)
{
	if (gamepad_action == WINDOW_KEY_PRESSED)
	{
		if (is_binded(kUI_UP, id))
		{
			ActionRepeaters()->SetActionStarted(this, kUI_UP);
			UIList->MoveSelectionUp(true);
			if (smart_cast<CUIStalkerRankingElipsisItem*>(UIList->GetSelected()))
			{
				UIList->MoveSelectionUp(true);
			}
			return true;
		}
		else if (is_binded(kUI_DOWN, id))
		{
			ActionRepeaters()->SetActionStarted(this, kUI_DOWN);
			UIList->MoveSelectionDown(true);
			if (smart_cast<CUIStalkerRankingElipsisItem*>(UIList->GetSelected()))
			{
				UIList->MoveSelectionDown(true);
			}
			return true;
		}
	}

	return inherited::OnGamepadKeyAction(id, gamepad_action);
}

bool CUIStalkersRankingWnd::OnGamepadKeyHold(int id)
{
	if (is_binded(kUI_UP, id))
	{
		if (ActionRepeaters()->CanRepeatActionNow(this, kUI_UP))
		{
			UIList->MoveSelectionUp(false);
			if (smart_cast<CUIStalkerRankingElipsisItem*>(UIList->GetSelected()))
			{
				UIList->MoveSelectionUp(false);
			}
		}
		return true;
	}
	else if (is_binded(kUI_DOWN, id))
	{
		if (ActionRepeaters()->CanRepeatActionNow(this, kUI_DOWN))
		{
			UIList->MoveSelectionDown(false);
			if (smart_cast<CUIStalkerRankingElipsisItem*>(UIList->GetSelected()))
			{
				UIList->MoveSelectionDown(true);
			}
		}
		return true;
	}

	return inherited::OnGamepadKeyHold(id);
}

void add_human_to_top_list(ALife::_OBJECT_ID id)
{
	CSE_ALifeTraderAbstract* t	= ch_info_get_from_id(id);
	SStatData	d;
	d.id		= id;
	d.trader	= t;

	TOP_LIST::iterator it					= std::find(g_all_statistic_humans.begin(),g_all_statistic_humans.end(),d);

	if(it!=g_all_statistic_humans.end())
		g_all_statistic_humans.erase	(it);

	g_all_statistic_humans.push_back	(d);


//	t->m_rank	=	::Random.randI(20000);
}

void remove_human_from_top_list(ALife::_OBJECT_ID id)
{
	CSE_ALifeTraderAbstract* t				= ch_info_get_from_id(id);
	SStatData	d;
	d.id		= id;
	d.trader	= t;
	TOP_LIST::iterator it					= std::find(g_all_statistic_humans.begin(),g_all_statistic_humans.end(),d);
	if(it!=g_all_statistic_humans.end())
		g_all_statistic_humans.erase		(it);
}


CUIStalkerRankingInfoItem::CUIStalkerRankingInfoItem(CUIStalkersRankingWnd* w)
:m_StalkersRankingWnd(w),m_humanID(ALife::INVALID_OBJECT_ID)
{
}

void CUIStalkerRankingInfoItem::Init	(CUIXml* xml, const char* path, int idx)
{
	XML_NODE* _stored_root					= xml->GetLocalRoot();

	CUIXmlInit								xml_init;
	xml_init.InitWindow						(*xml, path, idx, this);

	xml->SetLocalRoot						(xml->NavigateToNode(path,idx));

	m_text1									= new CUIStatic(); m_text1->SetAutoDelete(true);
	AttachChild								(m_text1);
	xml_init.InitStatic						(*xml, "text_1", 0, m_text1);

	m_text2									= new CUIStatic(); m_text2->SetAutoDelete(true);
	AttachChild								(m_text2);
	xml_init.InitStatic						(*xml, "text_2", 0, m_text2);

	m_text3									= new CUIStatic(); m_text3->SetAutoDelete(true);
	AttachChild								(m_text3);
	xml_init.InitStatic						(*xml, "text_3", 0, m_text3);

	xml_init.InitAutoStaticGroup			(*xml, "auto", 0, this);

	m_stored_alpha							= color_get_A(m_text2->TextItemControl()->GetTextColor());
	xml->SetLocalRoot						(_stored_root);
}

void CUIStalkerRankingInfoItem::SetSelected	(bool b)
{
	CUISelectable::SetSelected				(b);
	m_text1->SetTextColor( subst_alpha(m_text1->TextItemControl()->GetTextColor(), b?255:m_stored_alpha ));
	m_text2->SetTextColor( subst_alpha(m_text2->TextItemControl()->GetTextColor(), b?255:m_stored_alpha ));
	m_text3->SetTextColor( subst_alpha(m_text3->TextItemControl()->GetTextColor(), b?255:m_stored_alpha ));
	if(b){ 
		m_StalkersRankingWnd->ShowHumanInfo			(m_humanID);
	}

}

bool CUIStalkerRankingInfoItem::OnMouseDown		(int mouse_btn)
{
	if(mouse_btn==MOUSE_1)
	{
		m_StalkersRankingWnd->GetTopList().SetSelected	(this);
		return true;
	}else
		return false;
}

void CUIStalkerRankingInfoItem::OnFocusReceive()
{
	inherited::OnFocusReceive();
	if (get_hint_wnd())
	{
		SetHintText();
	}
}

void CUIStalkerRankingInfoItem::SetHintText()
{
	const char* hint = "";

	luabind::functor<const char*> functorSetHint;
	if (ai().script_engine().functor("pda.coc_rankings_set_hint", functorSetHint))
		hint = functorSetHint(m_humanID);

	set_hint_text(hint);
}

CUIStalkerRankingElipsisItem::CUIStalkerRankingElipsisItem(CUIStalkersRankingWnd* w)
:inherited(w)
{}

void CUIStalkerRankingElipsisItem::SetSelected(bool b)
{
	CUISelectable::SetSelected(b);
}

bool CUIStalkerRankingElipsisItem::OnMouseDown(int mouse_btn)
{
	return false;
}
