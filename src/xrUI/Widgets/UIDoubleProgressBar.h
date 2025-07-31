// sea 08.01.2008
#pragma once

#include "UIWindow.h"
#include "UIProgressBar.h"

class UI_API CUIDoubleProgressBar :
	public CUIWindow
{
public: // func
						CUIDoubleProgressBar	();
	virtual				~CUIDoubleProgressBar	();

			void		InitFromXml		( CUIXml& xml_doc, LPCSTR path );
			void		SetTwoPos		( float cur_value, float compare_value );

	virtual CUIWindow* ui_cast_window() { return this; }

protected:
	CUIProgressBar		m_progress_one;
	CUIProgressBar		m_progress_two;

	u32					m_less_color; // red
	u32					m_more_color; // green

}; // class CUIDoubleProgressBar
