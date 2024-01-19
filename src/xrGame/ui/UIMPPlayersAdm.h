#pragma once

#include "UIWindow.h"
#include "UIWndCallback.h"

class CUIXml;
class CUIListBox;
class CUI3tButton;
class CUITrackBar;
class CUITextWnd;
class CUIComboBox;

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
		CUIComboBox*		m_pBanPlayerCombo;
		//CUITrackBar*		m_pBanTimeTrack;
		//CUITextWnd*			m_pBanTimeText;
public:
							CUIMpPlayersAdm();
							~CUIMpPlayersAdm();
				void		Init(CUIXml& xml_doc);
				void		RefreshPlayersList();
		virtual void 		SendMessage(CUIWindow* pWnd, s16 msg, void* pData = nullptr);
		void		FillPlayersList(u32 const);
				void		SetMaxPingLimit();
				void		SetMaxPingLimitText();
				void		GetSelPlayerScreenshot();
				void		GetSelPlayerConfig();
				void		KickSelPlayer();
				void		BanSelPlayer();
//				void		SetBanSelPlayerText();
};
