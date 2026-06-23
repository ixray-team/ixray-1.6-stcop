
#pragma once

#include "../../xrUI/Widgets/UIWindow.h"
#include "../../xrUI/Widgets/UIWndCallback.h"
#include "../encyclopedia_article_defs.h"
class CUINewsWnd;
class CUIFrameLineWnd;
class CUIFrameWindow;
class CUIAnimatedStatic;
class CUIStatic;
class CUITabControl;
class CUIScrollView;
class CUIListWnd;
class CEncyclopediaArticle;
class CUIGamepadLegend;

class CUIDiaryWnd: public CUIWindow, public CUIWndCallback
{
	typedef CUIWindow inherited;
	enum EDiaryFilter{
//			eInfo,
			eJournal=0,
			eNews,
			eNone
	};
protected:
	EDiaryFilter		m_currFilter;

	CUINewsWnd*			m_UINewsWnd;

	CUIWindow*			m_UILeftWnd;
	CUIWindow*			m_UIRightWnd;
	CUIFrameWindow*		m_UILeftFrame;
	CUIFrameLineWnd*	m_UILeftHeader;
	CUIFrameWindow*		m_UIRightFrame;
	CUIFrameLineWnd*	m_UIRightHeader;
	CUIAnimatedStatic*	m_UIAnimation;
	CUIListWnd*			m_SrcListWnd;
	CUIScrollView*		m_DescrView;
	CGameFont*			m_pTreeRootFont;
	u32					m_uTreeRootColor;
	CGameFont*			m_pTreeItemFont;
	u32					m_uTreeItemColor;

	xr_vector<Fvector2>	m_sign_places;
	CUIStatic*			m_updatedSectionImage;
	CUIStatic*			m_oldSectionImage;

	typedef xr_vector<CEncyclopediaArticle*>			ArticlesDB;
	typedef xr_vector<CEncyclopediaArticle*>::iterator	ArticlesDB_it;
	ArticlesDB				m_ArticlesDB;

			void 	OnFilterChanged			(CUIWindow*,void*);
			void 	OnSrcListItemClicked	(CUIWindow*,void*);
			void		UnloadJournalTab		();
			void		LoadJournalTab			(ARTICLE_DATA::EArticleType _type);
			void		UnloadNewsTab			();
			void		LoadNewsTab				();
			void		Reload					(EDiaryFilter new_filter);
			void		UpdateGamepadLegend		();
public:
	CUITabControl*		m_FilterTab;
	CUIGamepadLegend*	m_gamepad_legend = nullptr;
						CUIDiaryWnd				();
	virtual				~CUIDiaryWnd			();

	virtual void		SendMessage				(CUIWindow* pWnd, s16 msg, void* pData);
	virtual	void		Draw					();
	virtual	void		Reset					();

			void		Init					();
			void		AddNews					();
			void		MarkNewsAsRead			(bool status);
	virtual void		Show					(bool status);
	virtual void		Update					();
	virtual bool		OnGamepadKeyAction		(int id, EUIMessages gamepad_action);
	virtual bool		OnGamepadKeyHold		(int id);

	virtual CUIWindow* ui_cast_window() { return this; }
};

