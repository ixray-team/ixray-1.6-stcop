#pragma once
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/Widgets/UI3tButton.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"

#include "../InfoPortion.h"

#include "UICharacterInfo.h"
#include "UIItemInfo.h"
#include "../Phrase.h"
#include "../../xrUI/Widgets/UIWndCallback.h"

class CUIScrollView;
class CUIXml;
class CUITalkWnd;
class CUIPdaContactsWnd;
class CUIGamepadLegend;
class CUIQuestionItem;

class CUITalkDialogWnd final :
	public CUIWindow, 
	public CUIWndCallback
{
private:
	typedef CUIWindow inherited;
	CUIXml*			m_uiXml;
public:
				CUITalkDialogWnd		();
	virtual		~CUITalkDialogWnd		();
	

			void InitTalkDialogWnd		();
			void ReloadDialogLayout		(bool usePdaDialogXml, const CUIPdaContactsWnd* contacts = nullptr);

	virtual void SendMessage			(CUIWindow* pWnd, s16 msg, void* pData = NULL);

	virtual void Show();
	void Show(bool resetWidgetTree, bool notifyActorHud);
	void ShowForPdaEmbed();
	virtual void Hide();
	bool HasPdaDialogLayout() const { return m_hasPdaDialogLayout; }
	CUITalkWnd*	m_pParent;
	u32			GetHeaderColor()		{ return m_iNameTextColor; }
	CGameFont *	GetHeaderFont()			{ return m_pNameTextFont; }
	u32			GetOurReplicsColor()	{ return m_uOurReplicsColor; }

	bool				mechanic_mode; // for inventory upgrades

	bool				m_break_enabled = false;
	bool				m_trade_enabled = false;

	//номер выбранного вопроса
	shared_str			m_ClickedQuestionID;

	//список вопросов, которые мы можем задавать персонажу
	CUIStatic* UIDialogFrameTop;
	CUIStatic* UIDialogFrameBottom;

	CUIStatic*			UIStaticTop;
	CUIStatic*			UIStaticBottom;
	CUIFrameLineWnd*	UIDialogFrame;
	CUIFrameLineWnd*	UIOurPhrasesFrame;

	Fvector2			m_btn_pos[3];
	CUI3tButton			UIToTradeButton;
	CUI3tButton*		UIToExitButton;

	CUIStatic* UIOurIcon;
	CUIStatic* UIOthersIcon;
	CUICharacterInfo	UICharacterInfoLeft;
	CUICharacterInfo	UICharacterInfoRight;
	CUIGamepadLegend*	m_gamepad_legend = nullptr;
	CUIWindow*			m_gamepad_trade_hint = nullptr;
	CUIWindow*			m_gamepad_back_hint = nullptr;
	CUIWindow*			m_gamepad_log_hint = nullptr;

	bool				swapCharacterNames = false;

	void				AddQuestion			(const char* str, const char* value, int number, SPhraseInfo &phInfo);
	void				AddAnswer			(const char* SpeakerName, const char* str, bool bActor);
	void				AddIconedAnswer		(const char* caption, const char* text, const char* texture_name, const char* templ_name);
	void				AddIconedAnswer		(const char* text, const char* texture_name, Frect texture_rect, const char* templ_name);
	void				ClearAll			();
	bool				TryClearAll			();
	void				ClearQuestions		();
	bool				TryClearQuestions	();

	void				SetOsoznanieMode	(bool b);
	void				SetTradeMode		();
	void				UpdateButtonsLayout	(bool b_disable_break, bool trade_enabled);

	virtual CUIWindow* ui_cast_window() { return this; }

	void				SetFirstQuestionSelected();
	bool				OffsetQuestionSelection(bool next, bool bLoop);
	void				ResetQuestionSelection();
	void				UpdateQuestionSelection();
	bool				HasQuestionWithID(shared_str questionID);
	void				ScrollSelectionIntoView();
	void				ScrollLogUp();
	void				ScrollLogDown();
	bool				TryClickFinalizerQuestion();

protected:
	CUIQuestionItem*	GetQuestionItemByID(shared_str questionID);
	void				UpdateGamepadLegend();

private:
	CUIScrollView*			UIQuestionsList;
	CUIScrollView*			UIAnswersList;

	// Ўрифт и цвет текста с именем персонажа
	CGameFont			*m_pNameTextFont;
	u32					m_iNameTextColor;
	// ÷вет тeкста и шрифт наших реплик
	u32					m_uOurReplicsColor;

	void 		OnTradeClicked			(CUIWindow* w, void*);
	void 		OnUpgradeClicked		(CUIWindow* w, void*);
	void 		OnQuestionClicked		(CUIWindow* w, void*);
	void 		OnExitClicked			(CUIWindow* w, void*);

	bool m_usePdaDialogXml = false;
	bool m_hasPdaDialogLayout = false;
	bool _layoutXmlOwned = true;
	XML_NODE* _pdaDialogLayoutRoot = nullptr;
	void ReleaseLayoutXml();
	void BuildDialogLayout();
};


class CUIQuestionItem final :public CUIWindow, public CUIWndCallback
{
	typedef CUIWindow inherited;
	float			m_min_height;
	bool			m_is_finalizer = false;
public:
	CUIStatic*		m_num_text;
	CUI3tButton*	m_text;
	shared_str		m_s_value;
	float			m_fOffset;
	Fvector2		m_icon_size;
	float			m_fOffsetAfterIcon;
					CUIQuestionItem			(CUIXml* xml_doc, const char* path);
	void			Init					(const char* val, const char* text, bool isFinalizer);
	virtual void Update();
	bool			IsFinalizer() const		{ return m_is_finalizer;  }

	virtual void	SendMessage				(CUIWindow* pWnd, s16 msg, void* pData = NULL);
	void 	OnTextClicked			(CUIWindow* w, void*);

	virtual CUIWindow* ui_cast_window() { return this; }
};

class CUIAnswerItem :public CUIWindow
{
	typedef CUIWindow inherited;

	float			m_min_height;
	float			m_bottom_footer;
	CUIStatic*		m_text;
	CUIStatic*		m_name;
public:
					CUIAnswerItem			(CUIXml* xml_doc, const char* path);
	void			Init					(const char* text, const char* name);
};

class CUIAnswerItemIconed final :public CUIAnswerItem
{
	typedef CUIAnswerItem inherited;
	CUIStatic*		m_icon;

public:
					CUIAnswerItemIconed		(CUIXml* xml_doc, const char* path);
	void			Init					(const char* text, const char* name, const char* texture_name);
	virtual CUIWindow* ui_cast_window() { return this; }
    void			Init					(const char* text, const char* texture_name, Frect texture_rect);

};
