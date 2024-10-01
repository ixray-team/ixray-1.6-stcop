#pragma once

#include "../../xrUI/Widgets/UIStatic.h"

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

private:
	bool			m_bSelected;
	void			start_anim			();
	void			stop_anim			();
};
