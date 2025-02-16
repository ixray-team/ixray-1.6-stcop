#pragma once

#include "ui3tbutton.h"
#include "UIOptionsItem.h"

class UIHint;

class CUICheckButton : public CUI3tButton, public CUIOptionsItem
{
	typedef CUI3tButton			inherited;

public:
					CUICheckButton			();
	virtual			~CUICheckButton			();

	virtual void	Update					();

	// CUIOptionsItem
	virtual void	SetCurrentOptValue	();	// opt->current
	virtual void	SaveBackUpOptValue	();	// current->backup
	virtual void	SaveOptValue		();	// current->opt
	virtual void	UndoOptValue		();	// backup->current
	virtual bool	IsChangedOptValue	() const;	// backup!=current

	virtual void 	OnFocusReceive		();
	virtual void	OnFocusLost			();
	virtual void	Show				( bool status );
	virtual bool 	OnMouseAction				( float x, float y, EUIMessages mouse_action );
	virtual bool	OnMouseDown			( int mouse_btn );

			void InitCheckButton		(Fvector2 pos, Fvector2 size, LPCSTR texture_name);

	virtual void	Init_script				(float x, float y, float width, float height)
								{ InitCheckButton(Fvector2().set(x,y), Fvector2().set(width, height), "ui_checker");} //SkyLoader: for scripts

	void		InitTexture2				(LPCSTR texture_name);

	//состояние кнопки
	IC	bool	GetCheck					() const {return GetButtonState() == BUTTON_PUSHED;}
	IC	void	SetCheck(bool ch)
	{
		SetButtonState( ch ? BUTTON_PUSHED : BUTTON_NORMAL);
	}

	void SetDependControl(CUIWindow* pWnd);

private:
	bool							m_opt_backup_value;
	CUIWindow* m_pDependControl;

};