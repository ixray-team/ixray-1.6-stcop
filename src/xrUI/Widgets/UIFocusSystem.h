#pragma once

#include "../../xrCore/xrCore.h"
#include "../xrUIXmlParser.h"

class CUIWindow;
class CUIXml;

struct WND_SELECTOR_INFO
{
	CUIWindow*	pWnd;
	CUIWindow*	pWndLeft;
	CUIWindow*	pWndRight;
	CUIWindow*	pWndTop;
	CUIWindow*	pWndBottom;

	WND_SELECTOR_INFO(CUIWindow* pMain, CUIWindow* pl, CUIWindow* pr, CUIWindow* pt, CUIWindow* pb)
		:pWnd(pMain), pWndLeft(pl), pWndRight(pr), pWndTop(pt), pWndBottom(pb)
	{}
	WND_SELECTOR_INFO()
		:pWnd(nullptr), pWndLeft(nullptr), pWndRight(nullptr), pWndTop(nullptr), pWndBottom(nullptr)
	{}
};

UI_API int ReadWndSelectorsInfo(CUIXml& xml, const char* ui_path, xr_vector<WND_SELECTOR_INFO> &outInfos, xr_map<xr_string, CUIWindow*> &wndPointers);