#pragma once

#include "../xrScripts/script_export_space.h"
#include "../../xrUI/xrUIXmlParser.h"

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
class CUIEditBox;
class CUIMultiTextStatic;
class CUIAnimatedStatic;
class CUISleepStatic;
class CServerList;
class CUIMapList;
class CUITrackBar;
class CUIArrowStepper;
class CUIMapInfo;
class CUIMMShniaga;
class CUIScrollView;
class CUIListBox;
class CUIProgressBar;
class CUIListWnd;
class CUIStackPanel;

class CScriptXmlInit 
{
public:
	void ParseFile		(LPCSTR xml_file);
	void ParseDirFile(LPCSTR xml_dir, LPCSTR xml_file);

	bool NodeExist(LPCSTR path, int index);
	int GetNodesNum(LPCSTR path, int index, LPCSTR tag_name);
	bool NavigateToNode(LPCSTR path, int index);
	bool NavigateToNode_ByAttribute(LPCSTR tag_name, LPCSTR attrib_name, LPCSTR attrib_value);
	bool NavigateToNode_ByPath(LPCSTR path, int index, LPCSTR tag_name, LPCSTR attrib, LPCSTR attrib_value_pattern);
	void NavigateToRoot();
	LPCSTR ReadValue(LPCSTR path, int index);
	LPCSTR ReadAttribute(LPCSTR path, int index, LPCSTR attrib);

	void				InitWindow(LPCSTR path, int index, CUIWindow* pWnd);
	CUIFrameWindow*		InitFrame(LPCSTR path, CUIWindow* parent);
	CUIFrameLineWnd*	InitFrameLine(LPCSTR path, CUIWindow* parent);
	CUIEditBox*			InitEditBox(LPCSTR path, CUIWindow* parent);
	CUIStatic*			InitStatic(LPCSTR path, CUIWindow* parent);
	CUIStackPanel*		InitStackPanel(LPCSTR path, CUIWindow* parent);
	CUIStatic*			InitAnimStatic(LPCSTR path, CUIWindow* parent);
	CUIStatic*			InitSleepStatic(LPCSTR path, CUIWindow* parent);
	CUITextWnd*			InitTextWnd(LPCSTR path, CUIWindow* parent);
	CUICheckButton*		InitCheck(LPCSTR path, CUIWindow* parent);
	CUISpinNum*			InitSpinNum(LPCSTR path, CUIWindow* parent);
	CUISpinFlt*			InitSpinFlt(LPCSTR path, CUIWindow* parent);
	CUISpinText*		InitSpinText(LPCSTR path, CUIWindow* parent);
	CUIComboBox*		InitComboBox(LPCSTR path, CUIWindow* parent);
	CUI3tButton*		Init3tButton(LPCSTR path, CUIWindow* parent);
	CUIListWnd*			InitListWnd(pcstr path, CUIWindow* parent);

	CUITabControl*		InitTab(LPCSTR path, CUIWindow* parent);
	CServerList*		InitServerList(LPCSTR path, CUIWindow* parent);
	CUIMapList*			InitMapList(LPCSTR path, CUIWindow* parent);
	CUIMapInfo*			InitMapInfo(LPCSTR path, CUIWindow* parent);
	CUITrackBar*		InitTrackBar(LPCSTR path, CUIWindow* parent);
	CUIArrowStepper*	InitArrowStepper(LPCSTR path, CUIWindow* parent);
	CUIEditBox*			InitCDkey(LPCSTR path, CUIWindow* parent);
	CUIEditBox*			InitMPPlayerName(LPCSTR path, CUIWindow* parent);
	CUIMMShniaga*		InitMMShniaga(LPCSTR path, CUIWindow* parent);
	CUIWindow*			InitKeyBinding(LPCSTR path, CUIWindow* parent);
	CUIScrollView*		InitScrollView(LPCSTR path, CUIWindow* parent);
	CUIListBox*			InitListBox(LPCSTR path, CUIWindow* parent);
	CUIProgressBar*		InitProgressBar(LPCSTR path, CUIWindow* parent);
protected:
	CUIXml	m_xml;
public:
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
