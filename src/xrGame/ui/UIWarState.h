////////////////////////////////////////////////////////////////////////////
//	Module 		: UIWarState.h
//	Created 	: 15.04.2008
//	Author		: Evgeniy Sokolov
//	Description : UI war state (PDA) window class
////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../../xrUI/Widgets/UIHint.h"

class CUIXml;
class CUIStatic;
class CUIFrameWindow;

class UIWarState final : public UIHintWindow
{
private:
	typedef UIHintWindow	inherited;

	CUIStatic*		m_static;

public:
					UIWarState	() = default;
	virtual			~UIWarState	() = default;

			void	InitXML			( CUIXml& xml, const char* att_name, CUIWindow* parent );
			void	ClearInfo		();
			bool	UpdateInfo		( const char* icon, const char* hint_text );
	virtual void	OnFocusReceive	();
			void	ToggleHint		();		

	virtual CUIWindow* ui_cast_window() { return this; }

	CUIFrameWindow* m_frame_selected = nullptr;

protected:

}; // class UIWarState

