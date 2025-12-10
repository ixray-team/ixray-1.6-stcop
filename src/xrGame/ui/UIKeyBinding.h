#pragma once
#include "../../xrUI/Widgets/UIWindow.h"
//#include "UIListWnd.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/Widgets/UIEditBox.h"

class CUIXml;
class CUIScrollView;
class CUIStatic;
class CUIEditKeyBind;

class CUIKeyBinding final : public CUIWindow
{
public:
					CUIKeyBinding			();
	void			InitFromXml				(CUIXml& xml_doc, LPCSTR path);
#ifdef DEBUG
	void			CheckStructure			(CUIXml& xml_doc);
	bool			IsActionExist			(LPCSTR action, CUIXml& xml_doc);
#endif

	virtual CUIWindow* ui_cast_window() { return this; }
	virtual void		Update					();

protected:
	void			FillUpList				(CUIXml& xml_doc, LPCSTR path);
	void			UpdateQuickSlotsBindingState();

	CUIFrameLineWnd	m_header[3];
	CUIFrameWindow	m_frame;
	CUIScrollView*	m_scroll_wnd;

	// Store references to quick slots binding elements for dynamic updates
	CUIStatic*		m_quickSlotsItem;
	CUIEditKeyBind* m_quickSlotsKey1;
	CUIEditKeyBind* m_quickSlotsKey2;
};