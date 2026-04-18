#include "StdAfx.h"
#include "UIFocusSystem.h"



UI_API int ReadWndSelectorsInfo(CUIXml& xml, const char* ui_path, xr_vector<WND_SELECTOR_INFO> &outInfos, xr_map<xr_string, CUIWindow*> &wndPointers)
{
	XML_NODE* startNode = xml.NavigateToNode(ui_path);
	if (!startNode)
		return 1;

	const char* pTagName = "wnd_selector_info";
	int tagIndex = 0;
	while (xml.NavigateToNode(startNode, pTagName, tagIndex))
	{
		WND_SELECTOR_INFO wsi;
		xr_string attrib;
	
		attrib = xml.ReadAttrib(startNode, pTagName, tagIndex, "wnd");
		if (attrib != "" && wndPointers.find(attrib.c_str()) != wndPointers.end())
		{
			wsi.pWnd = wndPointers[attrib.c_str()];
			// wsi.pWnd->SetWindowName(attrib.c_str()); // Use this for tests only, this code is shared in 3 games, we dont want to overwrite names

			attrib = xml.ReadAttrib(startNode, pTagName, tagIndex, "wnd_l");
			if (attrib != "" && wndPointers.find(attrib.c_str()) != wndPointers.end())
				wsi.pWndLeft = wndPointers[attrib.c_str()];

			attrib = xml.ReadAttrib(startNode, pTagName, tagIndex, "wnd_r");
			if (attrib != "" && wndPointers.find(attrib.c_str()) != wndPointers.end())
				wsi.pWndRight = wndPointers[attrib.c_str()];

			attrib = xml.ReadAttrib(startNode, pTagName, tagIndex, "wnd_t");
			if (attrib != "" && wndPointers.find(attrib.c_str()) != wndPointers.end())
				wsi.pWndTop = wndPointers[attrib.c_str()];

			attrib = xml.ReadAttrib(startNode, pTagName, tagIndex, "wnd_b");
			if (attrib != "" && wndPointers.find(attrib.c_str()) != wndPointers.end())
				wsi.pWndBottom = wndPointers[attrib.c_str()];

			VERIFY(wsi.pWndLeft || wsi.pWndRight || wsi.pWndTop || wsi.pWndBottom);
			outInfos.push_back(wsi);
		}

		++tagIndex;
	}

	return 0;
}