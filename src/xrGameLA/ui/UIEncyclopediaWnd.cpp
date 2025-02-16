//=============================================================================
//  Filename:   UIEncyclopediaWnd.cpp
//	Created by Roman E. Marchenko, vortex@gsc-game.kiev.ua
//	Copyright 2004. GSC Game World
//	---------------------------------------------------------------------------
//  Encyclopedia window
//=============================================================================

#include "StdAfx.h"
#include "UIEncyclopediaWnd.h"
#include "UIXmlInit.h"
#include "UIFrameWindow.h"
#include "UIFrameLineWnd.h"
#include "UIAnimatedStatic.h"
#include "UIScrollView.h"
#include "UITreeViewBoxItem.h"
#include "UIPdaAux.h"
#include "UIEncyclopediaArticleWnd.h"
#include "../encyclopedia_article.h"
#include "../alife_registry_wrappers.h"
#include "../actor.h"
#include "../object_broker.h"
#include "UIListBox.h"

#define				ENCYCLOPEDIA_DIALOG_XML		"encyclopedia.xml"

CUIEncyclopediaWnd::CUIEncyclopediaWnd()
{
	prevArticlesCount	= 0;
}

CUIEncyclopediaWnd::~CUIEncyclopediaWnd()
{
	DeleteArticles();
}


void CUIEncyclopediaWnd::Init()
{
	CUIXml		uiXml;
	uiXml.Load	(CONFIG_PATH, UI_PATH, ENCYCLOPEDIA_DIALOG_XML);

	CUIXmlInit	xml_init;

	CUIXmlInit::InitWindow		(uiXml, "main_wnd", 0, this);

	// Load xml data
	UIEncyclopediaIdxBkg		= new CUIFrameWindow(); UIEncyclopediaIdxBkg->SetAutoDelete(true);
	AttachChild(UIEncyclopediaIdxBkg);
	CUIXmlInit::InitFrameWindow(uiXml, "right_frame_window", 0, UIEncyclopediaIdxBkg);

	CUIXmlInit::InitFont(uiXml, "tree_item_font", 0, m_uTreeItemColor, m_pTreeItemFont);
	R_ASSERT(m_pTreeItemFont);
	CUIXmlInit::InitFont(uiXml, "tree_root_font", 0, m_uTreeRootColor, m_pTreeRootFont);
	R_ASSERT(m_pTreeRootFont);


	UIEncyclopediaIdxHeader		= new CUIFrameLineWnd(); UIEncyclopediaIdxHeader->SetAutoDelete(true);
	UIEncyclopediaIdxBkg->AttachChild(UIEncyclopediaIdxHeader);
	CUIXmlInit::InitFrameLine(uiXml, "right_frame_line", 0, UIEncyclopediaIdxHeader);

	UIAnimation					= new CUIAnimatedStatic(); UIAnimation->SetAutoDelete(true);
	UIEncyclopediaIdxHeader->AttachChild(UIAnimation);
	CUIXmlInit::InitAnimatedStatic(uiXml, "a_static", 0, UIAnimation);

	UIEncyclopediaInfoBkg		= new CUIFrameWindow();UIEncyclopediaInfoBkg->SetAutoDelete(true);
	AttachChild(UIEncyclopediaInfoBkg);
	CUIXmlInit::InitFrameWindow(uiXml, "left_frame_window", 0, UIEncyclopediaInfoBkg);

	UIEncyclopediaInfoHeader	= new CUIFrameLineWnd();UIEncyclopediaInfoHeader->SetAutoDelete(true);
	UIEncyclopediaInfoBkg->AttachChild(UIEncyclopediaInfoHeader);

//	UIEncyclopediaInfoHeader->UITitleText.SetElipsis(CUIStatic::eepBegin, 20);
	CUIXmlInit::InitFrameLine(uiXml, "left_frame_line", 0, UIEncyclopediaInfoHeader);

	UIArticleHeader				= new CUIStatic(); UIArticleHeader->SetAutoDelete(true);
	UIEncyclopediaInfoBkg->AttachChild(UIArticleHeader);
	CUIXmlInit::InitStatic(uiXml, "article_header_static", 0, UIArticleHeader);

	UIIdxList					= new CUIListBox(); UIIdxList->SetAutoDelete(true);
	UIEncyclopediaIdxBkg->AttachChild(UIIdxList);
	CUIXmlInit::InitListBox(uiXml, "idx_list", 0, UIIdxList);
	UIIdxList->SetMessageTarget(this);
	//UIIdxList->EnableScrollBar(true);

	UIInfoList					= new CUIScrollView(); UIInfoList->SetAutoDelete(true);
	UIEncyclopediaInfoBkg->AttachChild(UIInfoList);
	CUIXmlInit::InitScrollView(uiXml, "info_list", 0, UIInfoList);

	CUIXmlInit::InitAutoStatic(uiXml, "left_auto_static", UIEncyclopediaInfoBkg);
	CUIXmlInit::InitAutoStatic(uiXml, "right_auto_static", UIEncyclopediaIdxBkg);
}

#include "../string_table.h"
void CUIEncyclopediaWnd::SendMessage(CUIWindow *pWnd, s16 msg, void* pData)
{
	if (UIIdxList == pWnd && LIST_ITEM_CLICKED == msg)
	{
		CUITreeViewBoxItem *pTVItem = (CUITreeViewBoxItem*)(pData);
		R_ASSERT		(pTVItem);
		
		if( pTVItem->vSubItems.size() )
		{
			CEncyclopediaArticle* A = m_ArticlesDB[pTVItem->vSubItems[0]->GetArticleValue()];

			xr_string caption		= ALL_PDA_HEADER_PREFIX;
			caption					+= "/";
			caption					+= CStringTable().translate(A->data()->group).c_str();

			UIEncyclopediaInfoHeader->UITitleText.SetText(caption.c_str());
			UIArticleHeader->TextItemControl()->SetTextST(*(A->data()->group));
			SetCurrentArtice		(NULL);
		}else
		{
			int idx = pTVItem->GetArticleValue();
			if (idx==-1) return;
			CEncyclopediaArticle* A = m_ArticlesDB[idx];
			xr_string caption		= ALL_PDA_HEADER_PREFIX;
			caption					+= "/";
			caption					+= CStringTable().translate(A->data()->group).c_str();
			caption					+= "/";
			caption					+= CStringTable().translate(A->data()->name).c_str();

			UIEncyclopediaInfoHeader->UITitleText.SetText(caption.c_str());
			SetCurrentArtice		(pTVItem);
			UIArticleHeader->TextItemControl()->SetTextST(*(A->data()->name));
		}
	}

	inherited::SendMessage(pWnd, msg, pData);
}

void CUIEncyclopediaWnd::Draw()
{
	
if(	m_flags.test(eNeedReload )){
	if(Actor()->encyclopedia_registry->registry().objects_ptr() && Actor()->encyclopedia_registry->registry().objects_ptr()->size() > prevArticlesCount)
	{
		ARTICLE_VECTOR::const_iterator it = Actor()->encyclopedia_registry->registry().objects_ptr()->begin();
		std::advance(it, prevArticlesCount);
		for(; it != Actor()->encyclopedia_registry->registry().objects_ptr()->end(); it++)
		{
			if (ARTICLE_DATA::eEncyclopediaArticle == it->article_type)
			{
				AddArticle(it->article_id, it->readed);
			}
		}
		prevArticlesCount = Actor()->encyclopedia_registry->registry().objects_ptr()->size();
	}
	
	m_flags.set(eNeedReload, FALSE);
	}

	inherited::Draw();
}

void CUIEncyclopediaWnd::ReloadArticles()
{
	m_flags.set(eNeedReload, TRUE);
}


void CUIEncyclopediaWnd::Show(bool status)
{
	if (status)
		ReloadArticles();

	inherited::Show(status);
}


bool CUIEncyclopediaWnd::HasArticle(shared_str id)
{
	ReloadArticles();
	for(std::size_t i = 0; i<m_ArticlesDB.size(); ++i)
	{
		if(m_ArticlesDB[i]->Id() == id) return true;
	}
	return false;
}


void CUIEncyclopediaWnd::DeleteArticles()
{
	UIIdxList->RemoveAll();
	delete_data			(m_ArticlesDB);
}

void CUIEncyclopediaWnd::SetCurrentArtice(CUITreeViewBoxItem *pTVItem)
{
	UIInfoList->ScrollToBegin();
	UIInfoList->Clear();

	if(!pTVItem) return;

	// для начала проверим, что нажатый элемент не рутовый
	if (!pTVItem->IsRoot())
	{

		CUIEncyclopediaArticleWnd*	article_info = new CUIEncyclopediaArticleWnd();
		article_info->Init			("encyclopedia_item.xml","encyclopedia_wnd:objective_item");
		article_info->SetArticle	(m_ArticlesDB[pTVItem->GetArticleValue()]);
		UIInfoList->AddWindow		(article_info, true);

		// Пометим как прочитанную
		if (!pTVItem->IsArticleReaded())
		{
			if(Actor()->encyclopedia_registry->registry().objects_ptr())
			{
				for(ARTICLE_VECTOR::iterator it = Actor()->encyclopedia_registry->registry().objects().begin();
					it != Actor()->encyclopedia_registry->registry().objects().end(); it++)
				{
					if (ARTICLE_DATA::eEncyclopediaArticle == it->article_type &&
						m_ArticlesDB[pTVItem->GetArticleValue()]->Id() == it->article_id)
					{
						it->readed = true;
						break;
					}
				}
			}
		}
	}
}

void CUIEncyclopediaWnd::AddArticle(shared_str article_id, bool bReaded)
{
	for(std::size_t i = 0; i<m_ArticlesDB.size(); i++)
	{
		if(m_ArticlesDB[i]->Id() == article_id) return;
	}

/*	// Добавляем элемент
	m_ArticlesDB.resize(m_ArticlesDB.size() + 1);
	CEncyclopediaArticle*& a = m_ArticlesDB.back();
	a = xr_new<CEncyclopediaArticle>();
	a->Load(article_id);
*/
	CEncyclopediaArticle* article = new CEncyclopediaArticle();
	article->Load(article_id);
	m_ArticlesDB.push_back(article);

	// Теперь создаем иерархию вещи по заданному пути

	CreateTreeBranch(article->data()->group, article->data()->name, UIIdxList, m_ArticlesDB.size() - 1, 
		m_pTreeRootFont, m_uTreeRootColor, m_pTreeItemFont, m_uTreeItemColor, bReaded);
}

void CUIEncyclopediaWnd::Reset()
{
	inherited::Reset	();
	ReloadArticles		();
}
