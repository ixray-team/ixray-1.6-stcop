#pragma once

#include "UIOptionsItem.h"
#include "UI_IB_Static.h"

class CUI3tButton;
class CUITrackButton;

enum ETrackBarMode
{
	eTrackBarModeInt,
	eTrackBarModeFloat,
	eTrackBarModeToken,
	eTrackBarModeBool
};

class UI_API CUITrackBar : 
	public CUI_IB_FrameLineWnd, 
	public CUIOptionsItem 
{
	friend class CUITrackButton;
public:
					CUITrackBar				();
	// CUIOptionsItem
	virtual void	SetCurrentOptValue		();	// opt->current
	virtual void	SaveBackUpOptValue		();	// current->backup
	virtual void	SaveOptValue			();	// current->opt
	virtual void	UndoOptValue			();	// backup->current
	virtual bool	IsChangedOptValue		() const;	// backup!=current
	
	virtual void	Draw					();
	virtual void	Update					();
	virtual bool	OnMouseAction			(float x, float y, EUIMessages mouse_action);
	virtual	void 	OnMessage				(LPCSTR message);

	// CUIWindow
			void	InitTrackBar			(Fvector2 pos, Fvector2 size);
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
			void	UpdateText				();
	CUI3tButton*	GetSlider				() { return m_pSlider; }

// modes code
			void	SetNumOfSigns			(int num_of_signs) { m_i_num_of_signs = num_of_signs; }
			void	SetTrackBarMode			(ETrackBarMode mode) { m_mode = mode; }
ETrackBarMode		GetTrackBarMode			() const { return m_mode; }
			bool	IsIntMode				() const { return m_mode == eTrackBarModeInt; }
			bool	IsFltMode				() const { return m_mode == eTrackBarModeFloat; }
			bool	IsTokenMode				() const { return m_mode == eTrackBarModeToken; }
			bool	IsBoolMode				() const { return m_mode == eTrackBarModeBool; }
			void	SetTokenValues			(xr_token* tokens);
			int		CurrentID				() const { return (m_i_val - 1); }
			void	SetCurrentID			(int val_id) { m_i_val = val_id + 1;}

public:
		IC	void	SetDrawingValue			(bool value) { m_bDrawValue = value; }

protected:
			void 	UpdatePos				();
			void 	UpdatePosRelativeToMouse();

	CUI3tButton*		m_pSlider;
	bool				m_b_invert;
	bool				m_b_mouse_capturer;
	bool				m_bDrawValue;
	int					m_i_num_of_signs;
	xr_token*			m_tokens;
	ETrackBarMode		m_mode;

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