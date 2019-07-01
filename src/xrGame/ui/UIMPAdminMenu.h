#pragma once

#include "UIDialogWnd.h"
#include "UIWindow.h"
#include "UIWndCallback.h"
class CUI3tButton;
class CUIStatic;
class CUITextWnd;
class CUITabControl;
class CUIXml;
class CUIMpPlayersAdm;
class CUIMpServerAdm;
class CUIListBox;
class CUI3tButton;
class CUITrackBar;

class CUIMpAdminMenu :	public CUIDialogWnd 
{
		typedef CUIWindow	inherited;
		CUIStatic*			m_pBack;
		CUITabControl*		m_pTabControl;
		CUIMpPlayersAdm*	m_pPlayersAdm;
		CUIMpServerAdm*		m_pServerAdm;
		CUIXml*				xml_doc;

		CUIWindow*			m_pActiveDialog;
		shared_str			m_sActiveSection;
		CUI3tButton*		m_pClose;
public:
							CUIMpAdminMenu();
		virtual				~CUIMpAdminMenu();
				void		Init();
		virtual void 		SendMessage(CUIWindow* pWnd, s16 msg, void* pData = NULL);
				void		SetActiveSubdialog	(const shared_str& section);
};

class CUIMpPlayersAdm :	public CUIWindow, public CUIWndCallback 
{
		typedef CUIWindow	inherited;
		CUIListBox*			m_pPlayersList;
		CUI3tButton*		m_pRefreshBtn;
		CUI3tButton*		m_pScreenAllBtn;
		CUI3tButton*		m_pConfigAllBtn;
		CUI3tButton*		m_pPingLimitBtn;
		CUITrackBar*		m_pPingLimitTrack;
		CUITextWnd*			m_pPingLimitText;
		CUI3tButton*		m_pScreenPlayerBtn;
		CUI3tButton*		m_pConfigPlayerBtn;
		CUI3tButton*		m_pKickPlayerBtn;
		CUI3tButton*		m_pBanPlayerBtn;
		CUITrackBar*		m_pBanTimeTrack;
		CUITextWnd*			m_pBanTimeText;
public:
							CUIMpPlayersAdm();
		virtual				~CUIMpPlayersAdm();
				void		Init(CUIXml& xml_doc);
				void		RefreshPlayersList();
		virtual void 		SendMessage(CUIWindow* pWnd, s16 msg, void* pData = NULL);
				void		SetMaxPingLimit();
				void		GetSelPlayerScreenshot();
				void		GetSelPlayerConfig();
				void		KickSelPlayer();
				void		BanSelPlayer();
};

class CUIMpServerAdm :	public CUIWindow, public CUIWndCallback 
{
		typedef CUIWindow	inherited;
public:
							CUIMpServerAdm();
		virtual				~CUIMpServerAdm();
				void		Init(CUIXml& xml_doc);
};
