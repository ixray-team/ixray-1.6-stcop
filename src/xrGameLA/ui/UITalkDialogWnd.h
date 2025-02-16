#pragma once


#include "../xrUI/Widgets/UIStatic.h"
#include "../xrUI/Widgets/UI3tButton.h"
#include "../xrUI/Widgets/UIFrameLineWnd.h"
#include "../InfoPortion.h"

#include "UICharacterInfo.h"
#include "UIItemInfo.h"

#include "../xrUI/Widgets/UIWndCallback.h"

class CUIScrollView;
class CUIXml;

class CUITalkDialogWnd: public CUIWindow, public CUIWndCallback
{
private:
	typedef CUIWindow inherited;
	CUIXml*			m_uiXml;
public:
	CUITalkDialogWnd();
	virtual ~CUITalkDialogWnd();
	

	virtual void 				Init(float x, float y, float width, float height);
	
	virtual void 				SendMessage(CUIWindow* pWnd, s16 msg, void* pData = nullptr);

	virtual void 				Show();
	virtual void 				Hide();

	u32			GetHeaderColor()		{ return m_iNameTextColor; }
	CGameFont *	GetHeaderFont()			{ return m_pNameTextFont; }
	u32			GetOurReplicsColor()	{ return m_uOurReplicsColor; }

	shared_str			m_ClickedQuestionID;

	//ñïèñîê âîïðîñîâ, êîòîðûå ìû ìîæåì çàäàâàòü ïåðñîíàæó

	//ýëåìåíòû èíòåðôåéñà äèàëîãà
	CUIFrameLineWnd		UIDialogFrame;
	CUIFrameLineWnd		UIOurPhrasesFrame;

	CUIStatic			UIStaticTop;
	CUIStatic			UIStaticBottom;

	CUI3tButton			UIToTradeButton;

	//èíôîðìàöèÿ î ïåðñîíàæàõ 
	CUIStatic			UIOurIcon;
	CUIStatic			UIOthersIcon;
	CUICharacterInfo	UICharacterInfoLeft;
	CUICharacterInfo	UICharacterInfoRight;

	void				AddQuestion			(LPCSTR str, LPCSTR value, int number);
	void				AddAnswer			(LPCSTR SpeakerName, const char* str, bool bActor);
	void				AddIconedAnswer		(LPCSTR text, LPCSTR texture_name, Frect texture_rect, LPCSTR templ_name);
	void				ClearAll			();
	void				ClearQuestions		();

	void				SetOsoznanieMode	(bool b);
	void				ShowTradeButton		(bool can_trade, bool can_upgrade);
private:
	CUIScrollView*			UIQuestionsList;
	CUIScrollView*			UIAnswersList;

	// Øðèôò è öâåò òåêñòà ñ èìåíåì ïåðñîíàæà
	CGameFont			*m_pNameTextFont;
	u32					m_iNameTextColor;
	// Öâåò òeêñòà è øðèôò íàøèõ ðåïëèê
	u32					m_uOurReplicsColor;

	void __stdcall		OnTradeClicked			(CUIWindow* w, void*);
	void __stdcall		OnQuestionClicked		(CUIWindow* w, void*);
	
};


class CUIQuestionItem :public CUIWindow, public CUIWndCallback
{
	typedef CUIWindow inherited;
	float			m_min_height;
public:
	CUITextWnd*		m_num_text;
	CUI3tButton*	m_text;
	shared_str		m_s_value;
					CUIQuestionItem			(CUIXml* xml_doc, LPCSTR path);
	void			Init					(LPCSTR val, LPCSTR text);

	virtual void	SendMessage				(CUIWindow* pWnd, s16 msg, void* pData = nullptr);
	void __stdcall	OnTextClicked			(CUIWindow* w, void*);
};

class CUIAnswerItem :public CUIWindow
{
	typedef CUIWindow inherited;

	float			m_min_height;
	float			m_bottom_footer;
	CUITextWnd*		m_text;
	CUITextWnd*		m_name;
public:
					CUIAnswerItem			(CUIXml* xml_doc, LPCSTR path);
	void			Init					(LPCSTR text, LPCSTR name);
};

class CUIAnswerItemIconed :public CUIAnswerItem
{
	typedef CUIAnswerItem inherited;
	CUIStatic*		m_icon;

public:
					CUIAnswerItemIconed		(CUIXml* xml_doc, LPCSTR path);
	void			Init					(LPCSTR text, LPCSTR texture_name, Frect texture_rect);

};
