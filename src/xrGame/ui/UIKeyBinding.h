#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
//#include "UIListWnd.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIEditBox.h"

class CUIXml;
class CUIScrollView;

class CUIKeyBinding : public CUIWindow 
{
public:
					CUIKeyBinding			();
	void			InitFromXml				(CUIXml& xml_doc, LPCSTR path);
#ifdef DEBUG
	void			CheckStructure			(CUIXml& xml_doc);
	bool			IsActionExist			(LPCSTR action, CUIXml& xml_doc);
#endif
protected:
	void			FillUpList				(CUIXml& xml_doc, LPCSTR path);

	CUIFrameLineWnd	m_header[3];
	CUIFrameWindow	m_frame;
	CUIScrollView*	m_scroll_wnd;
};