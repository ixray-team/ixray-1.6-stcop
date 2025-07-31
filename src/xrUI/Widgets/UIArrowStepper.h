////////////////////////////////////////////////////////////////////////////
//	Module 		: UIArrowStepper.cpp
//	Created 	: 29.01.2025
//	Modified 	: 02.06.2025
//	IXRay port	: 02.06.2025
//	Author		: Konstantin Tarasov
//	Description : Element for step-by-step value adjustment using arrows or direct clicks
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "UIOptionsItem.h"
#include "UI_IB_Static.h"
#include "UIInteractiveBackground.h"

class CUI3tButton;
class CUI_IB_Static;

enum EStepperMode
{
	eStepperModeInt,
	eStepperModeFloat,
	eStepperModeToken,
	eStepperModeBool
};

class UI_API CUIArrowStepper :
	public CUIWindow,
	public CUIOptionsItem 
{
public:
					CUIArrowStepper				();
	// CUIOptionsItem
	virtual void	SetCurrentOptValue		();	// opt->current
	virtual void	SaveBackUpOptValue		();	// current->backup
	virtual void	SaveOptValue			();	// current->opt
	virtual void	UndoOptValue			();	// backup->current
	virtual bool	IsChangedOptValue		() const;	// backup!=current
	
	virtual void	Draw					();
	virtual void	Show					(bool status);
	virtual void	SendMessage				(CUIWindow *pWnd, s16 msg, void* pData = 0);
	virtual void	Update					();
	virtual bool	OnMouseAction			(float x, float y, EUIMessages mouse_action);
	virtual	void 	OnMessage				(LPCSTR message);

	// CUIWindow
			void	InitArrowStepper		(Fvector2 pos, Fvector2 size);
	virtual void	Enable					(bool status);
			void	SetInvert				(bool v){m_b_invert=v;}
			bool	GetInvert				() const	{return m_b_invert;};
			void	SetStep					(float step);
			bool	GetCheck				() const;
			void	SetCheck				(bool b);
			int		GetIValue				(){return m_i_val;}
			float	GetFValue				(){return m_f_val;}
			void	SetOptIBounds			(int imin, int imax);
			void	SetOptFBounds			(float fmin, float fmax);

// modes code
			void	SetNumOfSigns			(int num_of_signs) { m_i_num_of_signs = num_of_signs; }
			void	SetStepperMode			(EStepperMode mode) { m_mode = mode; }
EStepperMode		GetStepperMode			() const { return m_mode; }
			bool	IsIntMode				() const { return m_mode == eStepperModeInt; }
			bool	IsFltMode				() const { return m_mode == eStepperModeFloat; }
			bool	IsTokenMode				() const { return m_mode == eStepperModeToken; }
			bool	IsBoolMode				() const { return m_mode == eStepperModeBool; }
			void	SetTokenValues			(xr_token* tokens);
			int		CurrentID				() const { return (m_i_val - 1); }
			void	SetCurrentID			(int val_id) { m_i_val = val_id + 1;}

protected:
			void	UpdateText				();
			void	ChangeOnEnd				(bool bRight);
			void	ChangeValue				(bool bAdd);

	bool				m_b_invert;
	bool				m_b_mouse_capturer;
	int					m_i_num_of_signs;
	xr_token*			m_tokens;
	EStepperMode		m_mode;
	CUIStatic*			m_TextVal;
	CUI3tButton*		m_LeftBtn;
	CUI3tButton*		m_RightBtn;
	CUI_IB_FrameLineWnd* m_FrameLine;

	union{
		struct{
			float				m_f_val;
			float				m_f_max;
			float				m_f_min;
			float				m_f_step;
			float				m_f_opt_backup_value;
		};
		struct{
			int					m_i_val;
			int					m_i_max;
			int					m_i_min;
			int					m_i_step;
			int					m_i_opt_backup_value;
		};
	};
};