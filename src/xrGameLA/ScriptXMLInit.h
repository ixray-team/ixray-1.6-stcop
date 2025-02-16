#pragma once

#include "script_export_space.h"
#include "ui\xrUIXmlParser.h"

class CUIWindow;
class CUIFrameWindow;
class CUIStatic;
class CUITextWnd;
class CUICheckButton;
class CUISpinNum;
class CUISpinText;
class CUISpinFlt;
class CUIComboBox;
class CUIButton;
class CUI3tButton;
class CUICheckButton;
class CUITabControl;
class CUIFrameLineWnd;
class CUILabel;
class CUIEditBox;
class CUIMultiTextStatic;
class CUIAnimatedStatic;
class CUIArtefactPanel;
class CUITrackBar;
class CUIMMShniaga;
class CUIScrollView;
class CUIListBox;
class CUIListWnd;
class CUIProgressBar;

class CScriptXmlInit 
{
public:
	void 				ParseFile		(LPCSTR xml_file);
	void 				ParseShTexInfo	(LPCSTR xml_file);
	void 				ParseFile		(LPCSTR xml_path, LPCSTR xml_file);
	void				InitWindow(LPCSTR path, int index, CUIWindow* pWnd);
	CUIFrameWindow*		InitFrame(LPCSTR path, CUIWindow* parent);
	CUIFrameLineWnd*	InitFrameLine(LPCSTR path, CUIWindow* parent);
	CUILabel*			InitLabel(LPCSTR path, CUIWindow* parent);
	CUIEditBox*			InitEditBox(LPCSTR path, CUIWindow* parent);
	CUIStatic*			InitStatic(LPCSTR path, CUIWindow* parent);
	CUIStatic*			InitAnimStatic(LPCSTR path, CUIWindow* parent);
	CUITextWnd*			InitTextWnd(LPCSTR path, CUIWindow* parent);
	CUICheckButton*		InitCheck(LPCSTR path, CUIWindow* parent);
	CUISpinNum*			InitSpinNum(LPCSTR path, CUIWindow* parent);
	CUISpinFlt*			InitSpinFlt(LPCSTR path, CUIWindow* parent);
	CUISpinText*		InitSpinText(LPCSTR path, CUIWindow* parent);
	CUIComboBox*		InitComboBox(LPCSTR path, CUIWindow* parent);
	CUIButton*			InitButton(LPCSTR path, CUIWindow* parent);
	CUI3tButton*		Init3tButton(LPCSTR path, CUIWindow* parent);
	CUITabControl*		InitTab(LPCSTR path, CUIWindow* parent);
	CUITrackBar*		InitTrackBar(LPCSTR path, CUIWindow* parent);
	CUIMMShniaga*		InitMMShniaga(LPCSTR path, CUIWindow* parent);
	CUIWindow*			InitKeyBinding(LPCSTR path, CUIWindow* parent);
	CUIScrollView*		InitScrollView(LPCSTR path, CUIWindow* parent);
	CUIListBox*			InitListBox(LPCSTR path, CUIWindow* parent);
	CUIListWnd*			InitListWnd(LPCSTR path, CUIWindow* parent);
	CUIProgressBar*		InitProgressBar(LPCSTR path, CUIWindow* parent);
	void				InitAutoStaticGroup(LPCSTR path, CUIWindow* parent);

protected:
	CUIXml	m_xml;
public:
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
