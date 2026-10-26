#pragma once

#include "../../xrUI/Widgets/UIDialogWnd.h"

class CUIStatic;
class CUI3tButton;
class CUIKickPlayer;
class CUIChangeMap;
class CUIChangeWeather;
class CUIChangeGameType;
class CUIXml;
class CUITextVote;

class CUIVotingCategory final : public CUIDialogWnd
{
private:
	typedef CUIDialogWnd inherited;
public:
						CUIVotingCategory	();
	virtual ~CUIVotingCategory	() override;

	virtual bool		OnKeyboardAction			(int dik, EUIMessages keyboard_action) override;
	virtual void		SendMessage			(CUIWindow* pWnd, s16 msg, void* pData = 0) override;

	void				OnBtn				(int i);
	void				OnBtnCancel			();

	virtual void		Update				() override;

	virtual CUIWindow* ui_cast_window() override { return this; }

protected:
	void				InitVotingCategory	();

	CUIStatic*			header;
	CUI3tButton*		btn[7];
	CUIStatic*			txt[7];
	CUIStatic*			bkgrnd;
	CUI3tButton*		btn_cancel;

	CUIKickPlayer*		kick;
	CUIChangeMap*		change_map;
	CUIChangeWeather*	change_weather;
	CUIChangeGameType*	change_gametype;
	CUIXml*				xml_doc;
};