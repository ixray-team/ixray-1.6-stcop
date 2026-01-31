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
class CUI3dStatic;
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
class CUITrackBar;
class CUIScrollView;
class CUIStackPanel;
class CUIGamepadLegend;

#include "Widgets/UIStackPanel.h"
#include "Widgets/UI3dStatic.h"

class UI_API UIHelper
{
public:
	UIHelper		() {};
	~UIHelper		() {};

	static	CUIStatic*			CreateStatic		( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical = true );
    static	CUI3dStatic*        Create3dStatic      ( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical = true );
	static	CUIStackPanel*		CreateStackPanel	( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical = true );
	static	CUIGamepadLegend*	CreateGamepadLegend	( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical = true );
	static	CUIProgressBar*		CreateProgressBar	( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUIProgressShape*	CreateProgressShape	( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUIFrameLineWnd*	CreateFrameLine		( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical = true );
	static	CUIFrameWindow*		CreateFrameWindow	( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical = true );
	static	CUI3tButton*		Create3tButton		( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUICheckButton*		CreateCheck			( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUIEditBox*			CreateEditBox		( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUITrackBar*		CreateTrackBar		( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent );
	static	CUIScrollView*		CreateScrollView	( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical = true );

	static	UIHint*				CreateHint			( CUIXml& xml, LPCSTR ui_path /*, CUIWindow* parent*/ );

}; // class UIHelper