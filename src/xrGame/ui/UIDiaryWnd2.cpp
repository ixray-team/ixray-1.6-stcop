#include "StdAfx.h"
#include "UIDiaryWnd.h"
#include "PdaUiSound.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "UINewsWnd.h"
#include "../../xrUI/Widgets/UIAnimatedStatic.h"
#include "../../xrUI/UIXmlInit.h"
#include "object_broker.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UIListWnd.h"
#include "../../xrUI/Widgets/UITreeViewItem.h"
#include "UIEncyclopediaArticleWnd.h"
#include "../Level.h"
#include "../Actor.h"
#include "../alife_registry_wrappers.h"
#include "../encyclopedia_article.h"
#include "UIPdaAux.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIGamepadLegend.h"

extern u32			g_pda_info_state;

CUIDiaryWnd::CUIDiaryWnd()
{
	m_currFilter	= eNone;
	ActionRepeaters()->Register(this, kUI_DOWN);
	ActionRepeaters()->Register(this, kUI_UP);
	ActionRepeaters()->Register(this, kUI_SECONDARY_DOWN);
	ActionRepeaters()->Register(this, kUI_SECONDARY_UP);
}

CUIDiaryWnd::~CUIDiaryWnd()
{
	ActionRepeaters()->UnregisterOwner(this);
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

void RearrangeTabButtonsLegacy(CUITabControl* pTab, xr_vector<Fvector2>& vec_sign_places);

void CUIDiaryWnd::Init()
{
	CUIXml uiXml;
	uiXml.Load(CONFIG_PATH, UI_PATH, "events_new.xml");
	CUIXmlInit xml_init;

	xml_init.InitWindow				(uiXml, "main_wnd", 0, this);

	m_UILeftFrame					= new CUIFrameWindow(); m_UILeftFrame->SetAutoDelete(true);
	xml_init.InitFrameWindow		(uiXml,"main_wnd:left_frame",0,m_UILeftFrame);
	AttachChild						(m_UILeftFrame);
	
	m_UILeftHeader					= new CUIFrameLineWnd(); m_UILeftHeader->SetAutoDelete(true);
	xml_init.InitFrameLine			(uiXml, "main_wnd:left_frame:left_frame_header", 0, m_UILeftHeader);
	m_UILeftFrame->AttachChild		(m_UILeftHeader);

	m_FilterTab						= new CUITabControl();m_FilterTab->SetAutoDelete(true);
	m_UILeftHeader->AttachChild		(m_FilterTab);
	xml_init.InitTabControl			(uiXml, "main_wnd:left_frame:left_frame_header:filter_tab", 0, m_FilterTab);
	m_FilterTab->SetWindowName		("filter_tab");
	Register						(m_FilterTab);
    AddCallbackStr					("filter_tab", TAB_CHANGED, CUIWndCallback::void_function(this, &CUIDiaryWnd::OnFilterChanged));

	m_UIAnimation					= new CUIAnimatedStatic(); m_UIAnimation->SetAutoDelete(true);
	xml_init.InitAnimatedStatic		(uiXml, "main_wnd:left_frame:left_frame_header:anim_static", 0, m_UIAnimation);
	m_UILeftHeader->AttachChild		(m_UIAnimation);


	m_UILeftWnd						= new CUIWindow(); m_UILeftWnd->SetAutoDelete(true);
	xml_init.InitWindow				(uiXml, "main_wnd:left_frame:work_area", 0, m_UILeftWnd);
	m_UILeftFrame->AttachChild		(m_UILeftWnd);

	m_SrcListWnd					= new CUIListWnd(); m_SrcListWnd->SetAutoDelete(false);
	xml_init.InitListWnd			(uiXml, "main_wnd:left_frame:work_area:src_list", 0, m_SrcListWnd);
	m_SrcListWnd->SetWindowName		("src_list");
  	Register						(m_SrcListWnd);
	AddCallbackStr					("src_list", LIST_ITEM_CLICKED, CUIWndCallback::void_function(this, &CUIDiaryWnd::OnSrcListItemClicked));

	xml_init.InitFont				(uiXml, "main_wnd:left_frame:work_area:src_list:tree_item_font", 0, m_uTreeItemColor, m_pTreeItemFont);
	R_ASSERT						(m_pTreeItemFont);
	xml_init.InitFont				(uiXml, "main_wnd:left_frame:work_area:src_list:tree_root_font", 0, m_uTreeRootColor, m_pTreeRootFont);
	R_ASSERT						(m_pTreeRootFont);

	m_UIRightFrame					= new CUIFrameWindow();		m_UIRightFrame->SetAutoDelete(true);
	xml_init.InitFrameWindow		(uiXml,"main_wnd:right_frame",0,m_UIRightFrame);
	AttachChild						(m_UIRightFrame);

	m_UIRightHeader					= new CUIFrameLineWnd();	m_UIRightHeader->SetAutoDelete(true);
	xml_init.InitFrameLine			(uiXml, "main_wnd:right_frame:right_frame_header", 0, m_UIRightHeader);
	m_UIRightFrame->AttachChild		(m_UIRightHeader);

	m_UIRightWnd						= new CUIWindow(); m_UIRightWnd->SetAutoDelete(true);
	xml_init.InitWindow				(uiXml, "main_wnd:right_frame:work_area", 0, m_UIRightWnd);
	m_UIRightFrame->AttachChild		(m_UIRightWnd);

	m_UINewsWnd						= new CUINewsWnd();m_UINewsWnd->SetAutoDelete(false);
	m_UINewsWnd->Init				();

	m_DescrView						= new CUIScrollView(); m_DescrView->SetAutoDelete(false);
	xml_init.InitScrollView			(uiXml, "main_wnd:right_frame:work_area:scroll_view", 0, m_DescrView);

	m_updatedSectionImage			= new CUIStatic();
	xml_init.InitStatic				(uiXml, "updated_section_static", 0, m_updatedSectionImage);

	m_oldSectionImage				= new CUIStatic();
	xml_init.InitStatic				(uiXml, "old_section_static", 0, m_oldSectionImage);
	m_gamepad_legend				= UIHelper::CreateGamepadLegend(uiXml, "gamepad_legend", this, false);

	RearrangeTabButtonsLegacy		(m_FilterTab, m_sign_places);

	if (m_pUiSounds)
	{
		m_pUiSounds->LoadSubdialog(uiXml, "main_wnd");
	}
}

void	CUIDiaryWnd::SendMessage			(CUIWindow* pWnd, s16 msg, void* pData)
{
	inherited::SendMessage(pWnd, msg, pData);
	CUIWndCallback::OnEvent(pWnd, msg, pData);
}

void CUIDiaryWnd::OnFilterChanged			(CUIWindow* w, void*)
{
	if (m_pUiSounds)
	{
		m_pUiSounds->Play(EPdaUiSound::Tab);
	}
	Reload( (EDiaryFilter)m_FilterTab->GetActiveIndex() );
}

void CUIDiaryWnd::Reload	(EDiaryFilter new_filter)
{
	if (m_currFilter == eNone)
	{
		m_currFilter = eJournal;
		LoadJournalTab(ARTICLE_DATA::eJournalArticle);
		m_FilterTab->SetActiveTabByIndex(eJournal);
		return;
	}

	switch (m_currFilter){
		case eJournal:
			UnloadJournalTab	();
			break;
		case eNews:
			UnloadNewsTab	();
			break;
	};

	m_currFilter = new_filter;

	switch (m_currFilter){
		case eJournal:
			LoadJournalTab	(ARTICLE_DATA::eJournalArticle);
			break;
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
	UpdateGamepadLegend();
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
	if (m_pUiSounds)
	{
		m_pUiSounds->Play(EPdaUiSound::ListSelect);
	}

	CUITreeViewItem*	pSelItem	= (CUITreeViewItem*)p;
	m_DescrView->Clear	();
	if (!pSelItem->IsRoot())
	{
		CUIEncyclopediaArticleWnd*	article_info = new CUIEncyclopediaArticleWnd();
		article_info->Init			("encyclopedia_item.xml","encyclopedia_wnd:objective_item");
		article_info->SetArticle	(m_ArticlesDB[pSelItem->GetValue()]);
		m_DescrView->AddWindow		(article_info, true);

		// Пометим как прочитанную
		if (!pSelItem->IsArticleReaded())
		{
			if (Actor()->encyclopedia_registry->registry().objects_ptr())
			{
				for (ARTICLE_VECTOR::iterator it = Actor()->encyclopedia_registry->registry().objects().begin();
					it != Actor()->encyclopedia_registry->registry().objects().end(); it++)
				{
					if (ARTICLE_DATA::eJournalArticle == it->article_type &&
						m_ArticlesDB[pSelItem->GetValue()]->Id() == it->article_id)
					{
						it->readed = true;
						break;
					}
				}
			}
		}
	}
}

void draw_sign(CUIStatic* s, Fvector2& pos);
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
	if (g_pda_info_state & pda_section::news)
		draw_sign								(m_updatedSectionImage, pos);
	else
		draw_sign								(m_oldSectionImage, pos);
	

	pos		= m_sign_places[eJournal];
	pos.add(tab_pos);
	if (g_pda_info_state & pda_section::journal)
		draw_sign								(m_updatedSectionImage, pos);
	else
		draw_sign								(m_oldSectionImage, pos);
}

void CUIDiaryWnd::Reset()
{
	inherited::Reset	();
}


bool CUIDiaryWnd::OnGamepadKeyAction(int id, EUIMessages gamepad_action)
{
	if (gamepad_action == WINDOW_KEY_PRESSED)
	{
		if (m_SrcListWnd->GetItemsCount())
		{
			if (is_binded(kUI_UP, id))
			{
				if (!any_binded_key_for_action_pressed_c(kUI_DOWN))
				{
					CUITreeViewItem* pItem = static_cast<CUITreeViewItem*>(m_SrcListWnd->GetItem(m_SrcListWnd->GetSelectedItem()));
					if (pItem)
					{
						pItem->UIBkg.TextureOff();
					}
					if (m_SrcListWnd->PrevItem(false, true))
					{
						pItem = static_cast<CUITreeViewItem*>(m_SrcListWnd->GetItem(m_SrcListWnd->GetSelectedItem()));
						pItem->UIBkg.TextureOn();
						if (!pItem->IsRoot())
						{
							pItem->MarkArticleAsRead(true);
						}
					}
				}
				ActionRepeaters()->SetActionStarted(this, kUI_UP);
				return true;
			}
			else if (is_binded(kUI_DOWN, id))
			{
				if (!any_binded_key_for_action_pressed_c(kUI_UP))
				{
					CUITreeViewItem* pItem = static_cast<CUITreeViewItem*>(m_SrcListWnd->GetItem(m_SrcListWnd->GetSelectedItem()));
					if (pItem)
					{
						pItem->UIBkg.TextureOff();
					}
					if (m_SrcListWnd->NextItem(false, true))
					{
						pItem = static_cast<CUITreeViewItem*>(m_SrcListWnd->GetItem(m_SrcListWnd->GetSelectedItem()));
						pItem->UIBkg.TextureOn();
						if (!pItem->IsRoot())
						{
							pItem->MarkArticleAsRead(true);
						}
					}
				}
				ActionRepeaters()->SetActionStarted(this, kUI_DOWN);
				return true;
			}
			else if (is_binded(kUI_SECONDARY_UP, id))
			{
				ActionRepeaters()->SetActionStarted(this, kUI_SECONDARY_UP);
				if (CUIScrollBar* bar = m_DescrView->ScrollBar())
				{
					bar->TryScrollDec();
				}
				return true;
			}
			else if (is_binded(kUI_SECONDARY_DOWN, id))
			{
				ActionRepeaters()->SetActionStarted(this, kUI_SECONDARY_DOWN);
				if (CUIScrollBar* bar = m_DescrView->ScrollBar())
				{
					bar->TryScrollInc();
				}
				return true;
			}
			else if (is_binded(kUI_LEFT, id) || is_binded(kUI_RIGHT, id) || is_binded(kUI_ACCEPT, id))
			{
				CUITreeViewItem* pItem = static_cast<CUITreeViewItem*>(m_SrcListWnd->GetItem(m_SrcListWnd->GetSelectedItem()));
				if (pItem)
				{
					if (pItem->IsRoot())
					{
						if (pItem->IsOpened())
							pItem->Close();
						else
							pItem->Open();
						m_SrcListWnd->ScrollToSelection();
					}
					else
					{
						pItem->UIBkg.TextureOff();
						pItem = pItem->GetOwner();
						pItem->Close();
						int idx = m_SrcListWnd->GetItemPos(pItem);
						m_SrcListWnd->SetSelectedItem(idx);
						m_SrcListWnd->ScrollToSelection();

						pItem->UIBkg.TextureOn();
					}
					return true;
				}
			}
		}
	}
	return inherited::OnGamepadKeyAction(id, gamepad_action);
}

bool CUIDiaryWnd::OnGamepadKeyHold(int id)
{
	if (is_binded(kUI_DOWN, id))
	{
		if (ActionRepeaters()->CanRepeatActionNow(this, kUI_DOWN) && !any_binded_key_for_action_pressed_c(kUI_UP))
		{
			CUITreeViewItem* pItem = static_cast<CUITreeViewItem*>(m_SrcListWnd->GetItem(m_SrcListWnd->GetSelectedItem()));
			if (pItem)
			{
				pItem->UIBkg.TextureOff();
			}
			if (m_SrcListWnd->NextItem())
			{
				pItem = static_cast<CUITreeViewItem*>(m_SrcListWnd->GetItem(m_SrcListWnd->GetSelectedItem()));
				pItem->UIBkg.TextureOn();
				if (!pItem->IsRoot())
				{
					pItem->MarkArticleAsRead(true);
				}
			}
			return true;
		}
	}
	else if (is_binded(kUI_UP, id))
	{
		if (ActionRepeaters()->CanRepeatActionNow(this, kUI_UP) && !any_binded_key_for_action_pressed_c(kUI_DOWN))
		{
			CUITreeViewItem* pItem = static_cast<CUITreeViewItem*>(m_SrcListWnd->GetItem(m_SrcListWnd->GetSelectedItem()));
			if (pItem)
			{
				pItem->UIBkg.TextureOff();
			}
			if (m_SrcListWnd->PrevItem())
			{
				pItem = static_cast<CUITreeViewItem*>(m_SrcListWnd->GetItem(m_SrcListWnd->GetSelectedItem()));
				pItem->UIBkg.TextureOn();
				if (!pItem->IsRoot())
				{
					pItem->MarkArticleAsRead(true);
				}
			}
			return true;
		}
	}
	else if (is_binded(kUI_SECONDARY_UP, id))
	{
		if (ActionRepeaters()->CanRepeatActionNow(this, kUI_SECONDARY_UP))
		{
			if (CUIScrollBar* bar = m_DescrView->ScrollBar())
			{
				bar->TryScrollDec();
			}
			return true;
		}
	}
	else if (is_binded(kUI_SECONDARY_DOWN, id))
	{
		if (ActionRepeaters()->CanRepeatActionNow(this, kUI_SECONDARY_DOWN))
		{
			if (CUIScrollBar* bar = m_DescrView->ScrollBar())
			{
				bar->TryScrollInc();
			}
			return true;
		}
	}

	return inherited::OnGamepadKeyHold(id);
}

void CUIDiaryWnd::Update()
{
	inherited::Update();
	UpdateGamepadLegend();
}

void CUIDiaryWnd::UpdateGamepadLegend()
{
	if (!m_gamepad_legend)
	{
		return;

	}
	CUIWindow* logToStart = m_gamepad_legend->FindChild("log_to_start");
	if (logToStart)
	{
		logToStart->Show(m_UINewsWnd->IsShown());
	}

	CUIWindow* logToEnd = m_gamepad_legend->FindChild("log_to_end");
	if (logToEnd)
	{
		logToEnd->Show(m_UINewsWnd->IsShown());
	}
}
