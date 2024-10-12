#pragma once

class CUIDialogWnd;
class CUIWindow;

class UI_API dlgItem
{
public:
	dlgItem			(CUIWindow* pWnd);
	CUIWindow*		wnd;
	bool			enabled;
	bool operator < (const dlgItem& itm) const;
};

class UI_API recvItem
{
public:
	enum{	eCrosshair		= (1<<0),
			eIndicators		= (1<<1)};
	recvItem		(CUIDialogWnd*);
	CUIDialogWnd*	m_item;
	Flags8			m_flags;
};

class UI_API CDialogHolder :
	public pureFrame
{
	//dialogs
	xr_vector<recvItem>										m_input_receivers;
	xr_vector<dlgItem>										m_dialogsToRender;
	xr_vector<dlgItem>										m_dialogsToRender_new;
	bool													m_b_in_update;

	void					StartMenu						(CUIDialogWnd* pDialog, bool bDoHideIndicators);
	void					StopMenu						(CUIDialogWnd* pDialog);
protected:
	void					DoRenderDialogs					();
	void					CleanInternals					();
public:
							CDialogHolder					();
	virtual					~CDialogHolder					();

	//dialogs
	void					OnExternalHideIndicators		();
	CUIDialogWnd*			TopInputReceiver				();
	void					SetMainInputReceiver			(CUIDialogWnd* ir, bool _find_remove);
	void					AddDialogToRender				(CUIWindow* pDialog);
	void					RemoveDialogToRender			(CUIWindow* pDialog);
	virtual void	_BCL	OnFrame							();
	virtual bool			UseIndicators					()						{return true;}

	virtual void			StartDialog						(CUIDialogWnd* pDialog, bool bDoHideIndicators);
	virtual void			StopDialog						(CUIDialogWnd* pDialog);
	virtual bool			IgnorePause						()	{return false;}

	virtual bool 			IR_UIOnKeyboardPress			(int dik);
	virtual bool 			IR_UIOnKeyboardRelease			(int dik);
	virtual bool 			IR_UIOnMouseMove				(int dx, int dy);
	virtual bool 			IR_UIOnMouseWheel				(int direction);
	virtual bool 			IR_UIOnKeyboardHold				(int dik);

};
