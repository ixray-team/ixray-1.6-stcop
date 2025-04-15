////////////////////////////////////////////////////////////////////////////
//	Module 		: UIHelper.h
//	Created 	: 17.01.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Helper class
////////////////////////////////////////////////////////////////////////////
#pragma once 

class CUIXml;
class CUIWindow;
class CUIStatic;
class CUITextWnd;
class CUIProgressBar;
class CUIProgressShape;
class CUIFrameLineWnd;
class CUIFrameWindow;
class CUI3tButton;
class CUICheckButton;
class UIHint;
class CUIDragDropListEx;
class CUIDragDropReferenceList;
class CUIEditBox;

class UI_API UIHelper
{
public:
	UIHelper		() {};
	~UIHelper		() {};

	static	CUIStatic*			CreateStatic		( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUITextWnd*			CreateTextWnd		( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUIProgressBar*		CreateProgressBar	( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUIProgressShape*	CreateProgressShape	( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUIFrameLineWnd*	CreateFrameLine		( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical = true );
	static	CUIFrameWindow*		CreateFrameWindow	( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical = true );
	static	CUI3tButton*		Create3tButton		( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUICheckButton*		CreateCheck			( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUIEditBox*			CreateEditBox		( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );

	static	UIHint*				CreateHint			( CUIXml& xml, LPCSTR ui_path /*, CUIWindow* parent*/ );

}; // class UIHelper