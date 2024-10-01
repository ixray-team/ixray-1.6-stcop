////////////////////////////////////////////////////////////////////////////
//	Module 		: UIHelper.h
//	Created 	: 17.01.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Helper class
////////////////////////////////////////////////////////////////////////////
#pragma once 
#include "../xrUI/UIXmlInit.h"
#include "../xrUI/UIHelper.h"

class UIHelperGame: 
	public UIHelper
{
public:
	static	CUIDragDropListEx*			CreateDragDropListEx( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUIDragDropReferenceList*	CreateDragDropReferenceList( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );

}; // class UIHelper

class CUIXmlInitGame: 
	public CUIXmlInit
{
public:

	static bool InitDragDropListEx(CUIXml& xml_doc, LPCSTR path, int index, CUIDragDropListEx* pWnd);
};