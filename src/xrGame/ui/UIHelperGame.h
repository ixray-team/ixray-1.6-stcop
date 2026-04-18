////////////////////////////////////////////////////////////////////////////
//	Module 		: UIHelper.h
//	Created 	: 17.01.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Helper class
////////////////////////////////////////////////////////////////////////////
#pragma once 
#include "../xrUI/UIXmlInit.h"
#include "../xrUI/UIHelper.h"

class UIHelperGame final :
	public UIHelper
{
public:
	static	CUIDragDropListEx*			CreateDragDropListEx( CUIXml& xml, const char* ui_path, CUIWindow* parent );
	static	CUIDragDropReferenceList*	CreateDragDropReferenceList( CUIXml& xml, const char* ui_path, CUIWindow* parent );

}; // class UIHelper

class CUIXmlInitGame final :
	public CUIXmlInit
{
public:

	static bool InitDragDropListEx(CUIXml& xml_doc, const char* path, int index, CUIDragDropListEx* pWnd);
};