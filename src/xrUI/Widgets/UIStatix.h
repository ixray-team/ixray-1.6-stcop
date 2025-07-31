#pragma once

#include "UIStatic.h"

class UI_API CUIStatix : 
	public CUIStatic
{
public:
					CUIStatix			();
	virtual			~CUIStatix			();

	virtual void 	Update				();
	virtual void 	OnFocusReceive		();
	virtual void 	OnFocusLost			();
	virtual bool 	OnMouseDown			(int mouse_btn);
			void 	SetSelectedState	(bool state);
			bool 	GetSelectedState	();

	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUIStatic* ui_cast_static() { return this; }

private:
	bool			m_bSelected;
	void			start_anim			();
	void			stop_anim			();
};
