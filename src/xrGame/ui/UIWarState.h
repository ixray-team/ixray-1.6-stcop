////////////////////////////////////////////////////////////////////////////
//	Module 		: UIWarState.h
//	Created 	: 15.04.2008
//	Author		: Evgeniy Sokolov
//	Description : UI war state (PDA) window class
////////////////////////////////////////////////////////////////////////////

#ifndef UI_WAR_STATE_H_INCLUDED
#define UI_WAR_STATE_H_INCLUDED

#include "../../xrUI/Widgets/UIHint.h"

class CUIXml;
class CUIStatic;

class UIWarState final : public UIHintWindow
{
private:
	typedef UIHintWindow	inherited;

	CUIStatic*		m_static;
//	shared_str		m_def_texture;
//	bool			m_installed;

public:
					UIWarState	();
	virtual			~UIWarState	() {};

			void	InitXML			( CUIXml& xml, const char* att_name, CUIWindow* parent );
			void	ClearInfo		();
			bool	UpdateInfo		( const char* icon, const char* hint_text );
	virtual	void	Draw			();

	virtual CUIWindow* ui_cast_window() { return this; }

protected:

}; // class UIWarState

#endif // UI_WAR_STATE_H_INCLUDED

