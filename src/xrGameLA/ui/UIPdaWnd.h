#ifndef __UIPDAWND_H__
#define __UIPDAWND_H__
#pragma once

#include "UIDialogWnd.h"
#include "UIPdaAux.h"
#include "../encyclopedia_article_defs.h"

class CInventoryOwner;
class CUIFrameLineWnd;
class CUIButton;
class CUITabControl;
class CUIStatic;
class CUIMapWnd;
class CUIEncyclopediaWnd;
class CUIDiaryWnd;
class CUIActorInfoWnd;
class CUIStalkersRankingWnd;
class CUIEventsWnd;
class CUIPdaContactsWnd;
class CUI3tButton;
class CUIDialogWndEx;

 

class CUIPdaWnd: public CUIDialogWnd
{
private:
	typedef CUIDialogWnd	inherited;
protected:
	//элементы декоративного интерфейса
	CUIFrameLineWnd*		UIMainButtonsBackground;
	CUIFrameLineWnd*		UITimerBackground;

	// кнопки PDA
	CUITabControl*			UITabControl;

	// Установить игровое время
	void					UpdateDateTime				();
protected:
	// Бэкграунд
	CUIStatic*				UIMainPdaFrame;

	// Текущий активный диалог
	CUIWindow*				m_pActiveDialog;
	EPdaTabs				m_pActiveSection;
	bool					bUpgraded;
private:
	bool					m_initialized;

public:
	// Поддиалоги PDA
	CUIMapWnd*				UIMapWnd;
	CUIPdaContactsWnd*		UIPdaContactsWnd;
	CUIEncyclopediaWnd*		UIEncyclopediaWnd;
	CUIDiaryWnd*			UIDiaryWnd;
	CUIActorInfoWnd*		UIActorInfo;
	CUIStalkersRankingWnd*	UIStalkersRanking;
	CUIEventsWnd*			UIEventsWnd;
	CUIDialogWndEx*			UIChatWnd;
	CUIDialogWndEx*			UISkillsWnd;
	CUIDialogWndEx*			UIDownloadsWnd;
	CUIDialogWndEx*			UIGamesWnd;
	CUIDialogWndEx*			UIMPlayerWnd;
	CUI3tButton*			m_pUIClose;

	virtual void			Reset						();

public:
							CUIPdaWnd					();
	virtual					~CUIPdaWnd					();
	
	virtual void 			Init						();

	virtual void 			SendMessage					(CUIWindow* pWnd, s16 msg, void* pData = nullptr);

	virtual void 			Update						();
	virtual void 			ShowDialog					(bool bDoHideIndicators);
	virtual void 			ShowDialog					(bool bDoHideIndicators, EPdaTabs section);
	virtual void 			HideDialog					();
	
	virtual void 			EnableSkills				(bool val);
	virtual void 			EnableDownloads				(bool val);

	virtual bool			OnMouseAction				(float x, float y, EUIMessages mouse_action) {CUIDialogWnd::OnMouseAction(x,y,mouse_action);return true;} //always true because StopAnyMove() == false
	
	void					SetActiveSubdialog			(EPdaTabs section);
	virtual bool			StopAnyMove					(){return false;}

			void			PdaContentsChanged			(pda_section::part type);
};

#endif
