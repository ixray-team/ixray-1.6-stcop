#pragma once
#include "../../xrUI/Widgets/UIDialogWnd.h"
#include "../../xrUI/Widgets/UIWndCallback.h"

class CUIXml;
class CUIGameLog;
class CUIEditBox;
class CUIStatic;

class CUIChatWnd final: public CUIDialogWnd, public CUIWndCallback
{
	typedef CUIDialogWnd inherited;

public:
						CUIChatWnd			();
	virtual void		Show				(bool status) override;
	virtual bool		NeedCursor			() {return false;}
	void				Init				(CUIXml& uiXml);
	void				SetEditBoxPrefix	(const char* prefix);
	void				ChatToAll			(bool b) { sendNextMessageToAll = b; }
	void				PendingMode			(bool const is_pending_mode);
	virtual bool		NeedCursor			()const override {return false;}
	virtual void		SendMessage			(CUIWindow* pWnd, s16 msg, void* pData = NULL) override;

	virtual CUIWindow* ui_cast_window() override { return this; }

protected:
	CUIEditBox*			UIEditBox;
	CUIStatic*			UIPrefix;

	bool				sendNextMessageToAll;
	bool				pendingGameMode;
	
	Frect				pending_prefix_rect;
	Frect				pending_edit_rect;
	
	Frect				inprogress_prefix_rect;
	Frect				inprogress_edit_rect;

	void 		OnChatCommit		(CUIWindow* w, void* d);
	void 		OnChatCancel		(CUIWindow* w, void* d);
};
