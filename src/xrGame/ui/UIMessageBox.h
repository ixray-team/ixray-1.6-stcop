#pragma once

#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrScripts/script_export_space.h"

class CUI3tButton;
class CUIEditBox;

class CUIMessageBox final : public CUIStatic
{
private:
	typedef CUIStatic inherited;
public:
				CUIMessageBox		();
	virtual		~CUIMessageBox		();

	//разновидности MessageBox
	typedef enum {		
		MESSAGEBOX_OK, 
		MESSAGEBOX_INFO,
		MESSAGEBOX_YES_NO, 
		MESSAGEBOX_YES_NO_CANCEL, 
		MESSAGEBOX_DIRECT_IP, 
		MESSAGEBOX_PASSWORD, 
		MESSAGEBOX_RA_LOGIN, 
		MESSAGEBOX_QUIT_WINDOWS, 
		MESSAGEBOX_QUIT_GAME,
		MESSAGEBOX_YES_NO_COPY
	} E_MESSAGEBOX_STYLE;

	virtual void InitMessageBox		(const char* xml_template);
			void Clear				();
	virtual void SetText			(const char* str);
	virtual const char* GetText			();
	const char*		 GetHost			();
	const char*		 GetPassword		();
	const char*		 GetUserPassword	();
	void		 SetUserPasswordMode(bool);
	void		 SetPasswordMode	(bool);
	E_MESSAGEBOX_STYLE GetBoxStyle	()			{return m_eMessageBoxStyle;};

	void		 SetTextEditURL		(const char* text);
	const char*		 GetTextEditURL		();

	virtual bool OnMouseAction			(float x, float y, EUIMessages mouse_action);
	virtual void SendMessage		(CUIWindow *pWnd, s16 msg, void *pData);

	void		OnYesOk				();
	void		OnNo				();

	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUIStatic* ui_cast_static() { return this; }

protected:
	xr_string	m_ret_val;
	CUI3tButton* m_UIButtonYesOk;
	CUI3tButton* m_UIButtonNo;
	CUI3tButton* m_UIButtonCancel;
	CUI3tButton* m_UIButtonCopy;

	CUIStatic*	m_UIStaticPicture;
	CUIStatic*	m_UIStaticText;
	CUIStatic*	m_UIStaticHost;
	CUIStatic*	m_UIStaticPass;
	CUIStatic*	m_UIStaticUserPass;
	CUIEditBox* m_UIEditHost;
	CUIEditBox* m_UIEditPass;
	CUIEditBox* m_UIEditUserPass;
	CUIEditBox* m_UIEditURL;
	 
	E_MESSAGEBOX_STYLE m_eMessageBoxStyle;
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
