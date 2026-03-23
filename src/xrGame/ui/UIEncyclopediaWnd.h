//=============================================================================
//  Filename:   UIEncyclopediaWnd.h
//	Created by Roman E. Marchenko, vortex@gsc-game.kiev.ua
//	Copyright 2004. GSC Game World
//	---------------------------------------------------------------------------
//  Encyclopedia window
//=============================================================================

#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../encyclopedia_article_defs.h"

class CEncyclopediaArticle;
class CUIFrameWindow;
class CUIFrameLineWnd;
class CUIAnimatedStatic;
class CUIStatic;
class CUIListWnd;
class CUIScrollView;
class CUITreeViewItem;
class CUIGamepadLegend;

class CUIEncyclopediaWnd final: public CUIWindow
{
private:
	typedef CUIWindow inherited;
	enum {eNeedReload=(1<<0),};
	Flags16				m_flags;
public:
						CUIEncyclopediaWnd			();
	virtual				~CUIEncyclopediaWnd			();

	virtual void		Init						();
	virtual void		Show						(bool status);
	virtual void		SendMessage					(CUIWindow *pWnd, s16 msg, void* pData = nullptr);
	virtual void		Draw						();
	virtual bool		OnGamepadKeyAction			(int id, EUIMessages gamepad_action);
	virtual bool		OnGamepadKeyHold			(int id);

	void				AddArticle					(shared_str, bool bReaded);
	void				DeleteArticles				();
	bool				HasArticle					(shared_str);

	void				ReloadArticles				();
	virtual void		Reset						();
protected:
	u32					prevArticlesCount;
	// Элементы графического оформления
	CUIFrameWindow*		m_background;
	CUIFrameLineWnd*	m_background2;
	CUIFrameWindow*		UIEncyclopediaIdxBkg;
	CUIFrameWindow*		UIEncyclopediaInfoBkg;
	CUIFrameLineWnd*	UIEncyclopediaIdxHeader;
	CUIFrameLineWnd*	UIEncyclopediaInfoHeader;
	CUIAnimatedStatic*	UIAnimation;
	CUIStatic*			UIArticleHeader;
	CUIGamepadLegend*	m_gamepad_legend = nullptr;

	// Хранилище статей
	typedef xr_vector<CEncyclopediaArticle*>			ArticlesDB;
	typedef ArticlesDB::iterator						ArticlesDB_it;

	ArticlesDB				m_ArticlesDB;
	CGameFont*				m_pTreeRootFont;
	u32						m_uTreeRootColor;
	CGameFont*				m_pTreeItemFont;
	u32						m_uTreeItemColor;

	CUIListWnd*				UIIdxList;
	CUIScrollView*			UIInfoList;

	void				OnTreeViewItemClicked(CUITreeViewItem* pTVItem);
	void				SetCurrentArtice(CUITreeViewItem *pTVItem);
	virtual CUIWindow* ui_cast_window() { return this; }
};
