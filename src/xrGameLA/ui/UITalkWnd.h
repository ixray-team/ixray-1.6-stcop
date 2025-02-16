#pragma once

#include "../xrUI/Widgets/UIDialogWnd.h"
#include "../xrUI/Widgets/UIStatic.h"
#include "../xrUI/Widgets/UIButton.h"
#include "../xrUI/Widgets/UIEditBox.h"
#include "../xrUI/Widgets/UIFrameWindow.h"


#include "../PhraseDialogDefs.h"

class CActor;
class CInventoryOwner;
class CPhraseDialogManager;
class CUITalkDialogWnd;
class CUITradeWnd;
class CUIUpgradeWnd;
///////////////////////////////////////
//
///////////////////////////////////////

class CUITalkWnd: public CUIDialogWnd
{
private:
	typedef CUIDialogWnd inherited;
	ref_sound			m_sound;
	void				PlaySnd					(LPCSTR text);
	void				StopSnd					();
public:
						CUITalkWnd();
	virtual				~CUITalkWnd();

	IC		bool		playing_sound			()		 { return !!m_sound._feedback(); }
	IC	CInventoryOwner*OthersInvOwner			() const { return m_pOthersInvOwner;	 };

			void		InitTalkWnd				();

	virtual bool		StopAnyMove				(){return true;}
	virtual void		SendMessage				(CUIWindow* pWnd, s16 msg, void* pData = nullptr);

	virtual void		Draw					();
	virtual void		Update					();

	virtual void		Show					(bool status);
	
	void				Stop					();					//deffered
	void				StopTalk				();

	void				UpdateQuestions();
	void				NeedUpdateQuestions();
	//èíèöèàëèçàöèè íà÷àëüíîãî äèàëîãà ñîáåñåäíèêà
	void				InitOthersStartDialog	();
	virtual bool		OnKeyboardAction				(int dik, EUIMessages keyboard_action);
	void				SwitchToTrade			();
	void				SwitchToUpgrade			();
	void				AddIconedMessage		(LPCSTR text, LPCSTR texture_name, Frect texture_rect, LPCSTR templ_name);

protected:
	//äèàëîã
	void				InitTalkDialog			();
	void				AskQuestion				();

	void				SayPhrase				(const shared_str& phrase_id);

	// Ôóíêöèè äîáàâëåíèÿ ñòðîê â ëèñòû âîïðîñîâ è îòâåòîâ
public:
	void				AddQuestion				(const shared_str& text, const shared_str& id, int number);
	void				AddAnswer				(const shared_str& text, LPCSTR SpeakerName);
	bool				b_disable_break;
protected:
	//äëÿ ðåæèìà òîðãîâëè
	CUITradeWnd*			UITradeWnd;
	CUIUpgradeWnd*			UIUpgradeWnd;
	CUITalkDialogWnd*		UITalkDialogWnd;

	CActor*				m_pActor;
	CInventoryOwner*	m_pOurInvOwner;
	CInventoryOwner*	m_pOthersInvOwner;
	
	CPhraseDialogManager* m_pOurDialogManager;
	CPhraseDialogManager* m_pOthersDialogManager;

	bool				m_bNeedToUpdateQuestions;

	//òåêóùèé äèàëîã, åñëè nullptr, òî ïåðåõîäèì â ðåæèì âûáîðà òåìû
	DIALOG_SHARED_PTR	m_pCurrentDialog;
	bool				TopicMode				();
	void				ToTopicMode				();
};