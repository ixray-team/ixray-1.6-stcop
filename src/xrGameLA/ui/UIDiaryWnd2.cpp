#include "stdafx.h"
#include "UIDiaryWnd.h"
#include "UIFrameWindow.h"
#include "UIFrameLineWnd.h"
#include "UINewsWnd.h"
#include "UIAnimatedStatic.h"
#include "UIXmlInit.h"
#include "../object_broker.h"
#include "UITabControl.h"
#include "UIScrollView.h"
#include "UITreeViewBoxItem.h"
#include "UIEncyclopediaArticleWnd.h"
#include "../level.h"
#include "../actor.h"
#include "../alife_registry_wrappers.h"
#include "../encyclopedia_article.h"
#include "UIPdaAux.h"
#include "UIListBox.h"
#include "UITabButton.h"

extern u32			g_pda_info_state;

CUIDiaryWnd::CUIDiaryWnd()
{
	m_currFilter	= eNone;
}

CUIDiaryWnd::~CUIDiaryWnd()
{
	delete_data(m_UINewsWnd);
	delete_data(m_SrcListWnd);
	delete_data(m_DescrView);
	delete_data(m_ArticlesDB);
	delete_data(m_updatedSectionImage);
	delete_data(m_oldSectionImage);
}

void CUIDiaryWnd::Show(bool status)
{
	inherited::Show		(status);
	if(status)
		Reload( (EDiaryFilter)m_FilterTab->GetActiveIndex() );
}


void draw_sign(CUIStatic* s, Fvector2& pos);
void RearrangeTabButtons(CUITabControl* pTab, xr_vector<Fvector2>& vec_sign_places);

void CUIDiaryWnd::Init()
{
	CUIXml			uiXml;
	uiXml.Load		(CONFIG_PATH, UI_PATH, "events_new.xml");
	CUIXmlInit		xml_init;

	xml_init.InitWindow				(uiXml, "main_wnd", 0, this);

	m_UILeftFrame					= new CUIFrameWindow(); m_UILeftFrame->SetAutoDelete(true);
	CUIXmlInit::InitFrameWindow		(uiXml,"main_wnd:left_frame",0,m_UILeftFrame);
	AttachChild						(m_UILeftFrame);
	
	m_UILeftHeader					= new CUIFrameLineWnd(); m_UILeftHeader->SetAutoDelete(true);
	CUIXmlInit::InitFrameLine			(uiXml, "main_wnd:left_frame:left_frame_header", 0, m_UILeftHeader);
	m_UILeftFrame->AttachChild		(m_UILeftHeader);

	m_FilterTab						= new CUITabControl();m_FilterTab->SetAutoDelete(true);
	m_UILeftHeader->AttachChild		(m_FilterTab);
	CUIXmlInit::InitTabControl			(uiXml, "main_wnd:left_frame:left_frame_header:filter_tab", 0, m_FilterTab);
	m_FilterTab->SetWindowName		("filter_tab");
	Register						(m_FilterTab);
    AddCallbackStr					("filter_tab",TAB_CHANGED,CUIWndCallback::void_function(this,&CUIDiaryWnd::OnFilterChanged));

	m_UIAnimation					= new CUIAnimatedStatic(); m_UIAnimation->SetAutoDelete(true);
	CUIXmlInit::InitAnimatedStatic		(uiXml, "main_wnd:left_frame:left_frame_header:anim_static", 0, m_UIAnimation);
	m_UILeftHeader->AttachChild		(m_UIAnimation);


	m_UILeftWnd						= new CUIWindow(); m_UILeftWnd->SetAutoDelete(true);
	CUIXmlInit::InitWindow				(uiXml, "main_wnd:left_frame:work_area", 0, m_UILeftWnd);
	m_UILeftFrame->AttachChild		(m_UILeftWnd);

	m_SrcListWnd					= new CUIListBox(); m_SrcListWnd->SetAutoDelete(false);
	CUIXmlInit::InitListBox			(uiXml, "main_wnd:left_frame:work_area:src_list", 0, m_SrcListWnd);
	m_SrcListWnd->SetWindowName		("src_list");
	Register						(m_SrcListWnd);
    AddCallbackStr					("src_list",LIST_ITEM_CLICKED,CUIWndCallback::void_function(this,&CUIDiaryWnd::OnSrcListItemClicked));

	CUIXmlInit::InitFont				(uiXml, "main_wnd:left_frame:work_area:src_list:tree_item_font", 0, m_uTreeItemColor, m_pTreeItemFont);
	R_ASSERT						(m_pTreeItemFont);
	CUIXmlInit::InitFont				(uiXml, "main_wnd:left_frame:work_area:src_list:tree_root_font", 0, m_uTreeRootColor, m_pTreeRootFont);
	R_ASSERT						(m_pTreeRootFont);

	m_UIRightFrame					= new CUIFrameWindow();		m_UIRightFrame->SetAutoDelete(true);
	xml_init.InitFrameWindow		(uiXml,"main_wnd:right_frame",0,m_UIRightFrame);
	AttachChild						(m_UIRightFrame);

	m_UIRightHeader					= new CUIFrameLineWnd();	m_UIRightHeader->SetAutoDelete(true);
	CUIXmlInit::InitFrameLine			(uiXml, "main_wnd:right_frame:right_frame_header", 0, m_UIRightHeader);
	m_UIRightFrame->AttachChild		(m_UIRightHeader);

	m_UIRightWnd						= new CUIWindow(); m_UIRightWnd->SetAutoDelete(true);
	CUIXmlInit::InitWindow				(uiXml, "main_wnd:right_frame:work_area", 0, m_UIRightWnd);
	m_UIRightFrame->AttachChild		(m_UIRightWnd);

	m_UINewsWnd						= new CUINewsWnd();m_UINewsWnd->SetAutoDelete(false);
	m_UINewsWnd->Init				();

	m_DescrView						= new CUIScrollView(); m_DescrView->SetAutoDelete(false);
	CUIXmlInit::InitScrollView			(uiXml, "main_wnd:right_frame:work_area:scroll_view", 0, m_DescrView);

	m_updatedSectionImage			= new CUIStatic();
	CUIXmlInit::InitStatic				(uiXml, "updated_section_static", 0, m_updatedSectionImage);

	m_oldSectionImage				= new CUIStatic();
	CUIXmlInit::InitStatic				(uiXml, "old_section_static", 0, m_oldSectionImage);

	RearrangeTabButtons				(m_FilterTab, m_sign_places);
}

void	CUIDiaryWnd::SendMessage			(CUIWindow* pWnd, s16 msg, void* pData)
{
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

void CUIDiaryWnd::OnFilterChanged			(CUIWindow* w, void*)
{
	Reload( (EDiaryFilter)m_FilterTab->GetActiveIndex() );
}

void CUIDiaryWnd::Reload	(EDiaryFilter new_filter)
{
//.	if(m_currFilter==new_filter) return;

	switch (m_currFilter){
		case eJournal:
			UnloadJournalTab	();
			break;
//		case eInfo:
//			UnloadInfoTab	();
//			break;
		case eNews:
			UnloadNewsTab	();
			break;
	};

	m_currFilter = new_filter;

	switch (m_currFilter){
		case eJournal:
			LoadJournalTab	(ARTICLE_DATA::eJournalArticle);
			break;
//		case eInfo:
//			LoadInfoTab		();
//			break;
		case eNews:
			LoadNewsTab	();
			break;
	};
}

void CUIDiaryWnd::AddNews	()
{
	m_UINewsWnd->AddNews	();
}

void CUIDiaryWnd::MarkNewsAsRead (bool status)
{

}

void CUIDiaryWnd::UnloadJournalTab		()
{
	m_UILeftWnd->DetachChild	(m_SrcListWnd);
	m_SrcListWnd->RemoveAll		();
	m_SrcListWnd->Show			(false);

	m_UIRightWnd->DetachChild	(m_DescrView);
	m_DescrView->Show			(false);
	delete_data					(m_ArticlesDB);
	m_DescrView->Clear			();
}

void CUIDiaryWnd::LoadJournalTab			(ARTICLE_DATA::EArticleType _type)
{
	delete_data					(m_ArticlesDB);

	m_UILeftWnd->AttachChild	(m_SrcListWnd);
	m_SrcListWnd->Show			(true);

	m_UIRightWnd->AttachChild	(m_DescrView);
	m_DescrView->Show			(true);

	if(Actor()->encyclopedia_registry->registry().objects_ptr())
	{
		ARTICLE_VECTOR::const_iterator it = Actor()->encyclopedia_registry->registry().objects_ptr()->begin();
		for(; it != Actor()->encyclopedia_registry->registry().objects_ptr()->end(); it++)
		{
			if (_type == it->article_type)
				
			{
				m_ArticlesDB.resize(m_ArticlesDB.size() + 1);
				CEncyclopediaArticle*& a = m_ArticlesDB.back();
				a = new CEncyclopediaArticle();
				a->Load(it->article_id);

				CreateTreeBranch(a->data()->group, a->data()->name, m_SrcListWnd, m_ArticlesDB.size()-1, 
					m_pTreeRootFont, m_uTreeRootColor, m_pTreeItemFont, m_uTreeItemColor, it->readed);
			}
		}
	}
	g_pda_info_state	&=	!pda_section::journal;

}

void CUIDiaryWnd::UnloadInfoTab	()
{
//	m_UIRightWnd->DetachChild	(m_videoWnd);
//	m_videoWnd->Hide			();
	UnloadJournalTab	();
}

void CUIDiaryWnd::LoadInfoTab	()
{
//	m_UIRightWnd->AttachChild	(m_videoWnd);
//	m_videoWnd->Show			();
	LoadJournalTab				(ARTICLE_DATA::eInfoArticle);
	g_pda_info_state			&= ~pda_section::info;
}


void CUIDiaryWnd::UnloadNewsTab	()
{
	m_UIRightWnd->DetachChild	(m_UINewsWnd);
	m_UINewsWnd->Show			(false);
}

void CUIDiaryWnd::LoadNewsTab	()
{
	m_UIRightWnd->AttachChild	(m_UINewsWnd);
	m_UINewsWnd->Show			(true);
	g_pda_info_state			&= ~pda_section::news;
}

void CUIDiaryWnd::OnSrcListItemClicked	(CUIWindow* w,void* p)
{
	CUITreeViewBoxItem*	pSelItem	= static_cast<CUITreeViewBoxItem*>(p);
	m_DescrView->Clear	();
	if (!pSelItem->IsRoot())
	{
		CUIEncyclopediaArticleWnd*	article_info = new CUIEncyclopediaArticleWnd();
		article_info->Init			("encyclopedia_item.xml","encyclopedia_wnd:objective_item");
		article_info->SetArticle	(m_ArticlesDB[pSelItem->GetArticleValue()]);
		m_DescrView->AddWindow		(article_info, true);

		// Пометим как прочитанную
		if (!pSelItem->IsArticleReaded())
		{
			if(Actor()->encyclopedia_registry->registry().objects_ptr())
			{
				for(ARTICLE_VECTOR::iterator it = Actor()->encyclopedia_registry->registry().objects().begin();
					it != Actor()->encyclopedia_registry->registry().objects().end(); it++)
				{
					if (ARTICLE_DATA::eJournalArticle == it->article_type &&
						m_ArticlesDB[pSelItem->GetArticleValue()]->Id() == it->article_id)
					{
						it->readed = true;
						break;
					}
				}
			}
		}
	}
}

void CUIDiaryWnd::Draw()
{
	inherited::Draw	();

	m_updatedSectionImage->Update				();
	m_oldSectionImage->Update					();

	Fvector2									tab_pos;
	m_FilterTab->GetAbsolutePos					(tab_pos);

	Fvector2 pos;

	pos		= m_sign_places[eNews];
	pos.add(tab_pos);
	if(g_pda_info_state&pda_section::news)
		draw_sign								(m_updatedSectionImage, pos);
	else
		draw_sign								(m_oldSectionImage, pos);
	

	pos		= m_sign_places[eJournal];
	pos.add(tab_pos);
	if(g_pda_info_state&pda_section::journal)
		draw_sign								(m_updatedSectionImage, pos);
	else
		draw_sign								(m_oldSectionImage, pos);
}

void CUIDiaryWnd::Reset()
{
	inherited::Reset	();
}

void draw_sign		(CUIStatic* s, Fvector2& pos)
{
	s->SetWndPos		(pos);
	s->Draw				();
}

void RearrangeTabButtons(CUITabControl* pTab, xr_vector<Fvector2>& vec_sign_places)
{
	TABS_VECTOR *	btn_vec		= pTab->GetButtonsVector();
	TABS_VECTOR::iterator it	= btn_vec->begin();
	TABS_VECTOR::iterator it_e	= btn_vec->end();
	vec_sign_places.clear		();
	vec_sign_places.resize		(btn_vec->size());

	Fvector2					pos;
	pos.set						((*it)->GetWndPos());
	Fvector2					sign_sz;
	sign_sz.set					(9.0f+3.0f, 11.0f);
	u32 idx						= 0;
	float	btn_text_len		= 0.0f;
	CUIStatic* st				= NULL;

	for(;it!=it_e;++it,++idx)
	{
		if(idx!=0)
		{
			st = new CUIStatic(); st->SetAutoDelete(true);pTab->AttachChild(st);
			st->TextItemControl()->SetFont((*it)->TextItemControl()->GetFont());
			st->TextItemControl()->SetTextColor	(color_rgba(90,90,90,255));
			st->TextItemControl()->SetText("//");
			st->SetWndSize		((*it)->GetWndSize());
			st->AdjustWidthToText();
			st->SetWndPos		(pos);
			pos.x				+= st->GetWndSize().x;
		}

		vec_sign_places[idx].set(pos);
		vec_sign_places[idx].y	+= iFloor(((*it)->GetWndSize().y - sign_sz.y)/2.0f);
		vec_sign_places[idx].y	= (float)iFloor(vec_sign_places[idx].y);
		pos.x					+= sign_sz.x;

		(*it)->SetWndPos		(pos);
		(*it)->AdjustWidthToText();
		btn_text_len			= (*it)->GetWndSize().x;
		pos.x					+= btn_text_len+3.0f;
	}

}