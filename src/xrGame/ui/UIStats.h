#pragma once

#include "../../xrUI/Widgets/UIScrollView.h"
#include "UIStatsPlayerList.h"

class CUIXml;
class CUIFrameWindow;

class CUIStats : 
	public CUIScrollView
{
public:
				CUIStats		();
	virtual		~CUIStats		();
	CUIWindow* InitStats		(CUIXml& xml_doc, LPCSTR path,int team);

	virtual CUIWindow* ui_cast_window() { return this; }
	virtual CUIScrollView* ui_cast_scroll_view() { return this; }
};