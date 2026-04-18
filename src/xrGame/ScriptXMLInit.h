#pragma once

#include "../xrScripts/script_export_space.h"
#include "../../xrUI/xrUIXmlParser.h"

class CUIWindow;
class CUIFrameWindow;
class CUIStatic;
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
class CUIArrow;
class CUIGamepadLegend;

class CScriptXmlInit 
{
public:
	void ParseFile		(const char* xml_file);
	void ParseDirFile(const char* xml_dir, const char* xml_file);

	bool NodeExist(const char* path, int index);
	int GetNodesNum(const char* path, int index, const char* tag_name);
	bool NavigateToNode(const char* path, int index);
	bool NavigateToNode_ByAttribute(const char* tag_name, const char* attrib_name, const char* attrib_value);
	bool NavigateToNode_ByPath(const char* path, int index, const char* tag_name, const char* attrib, const char* attrib_value_pattern);
	void NavigateToRoot();
	const char* ReadValue(const char* path, int index);
	const char* ReadAttribute(const char* path, int index, const char* attrib);
	u32 GetColor(const char* path, int index);

	void				InitWindow(const char* path, int index, CUIWindow* pWnd);
	CUIFrameWindow*		InitFrame(const char* path, CUIWindow* parent);
	CUIFrameLineWnd*	InitFrameLine(const char* path, CUIWindow* parent);
	CUIEditBox*			InitEditBox(const char* path, CUIWindow* parent);
	CUIStatic*			InitStatic(const char* path, CUIWindow* parent);
	CUIStackPanel*		InitStackPanel(const char* path, CUIWindow* parent);
	CUIGamepadLegend*	InitGamepadLegend(const char* path, CUIWindow* parent);
	CUIStatic*			InitAnimStatic(const char* path, CUIWindow* parent);
	CUIStatic*			InitSleepStatic(const char* path, CUIWindow* parent);
	CUICheckButton*		InitCheck(const char* path, CUIWindow* parent);
	CUISpinNum*			InitSpinNum(const char* path, CUIWindow* parent);
	CUISpinFlt*			InitSpinFlt(const char* path, CUIWindow* parent);
	CUISpinText*		InitSpinText(const char* path, CUIWindow* parent);
	CUIComboBox*		InitComboBox(const char* path, CUIWindow* parent);
	CUI3tButton*		Init3tButton(const char* path, CUIWindow* parent);
	CUIListWnd*			InitListWnd(const char* path, CUIWindow* parent);

	CUITabControl*		InitTab(const char* path, CUIWindow* parent);
	CServerList*		InitServerList(const char* path, CUIWindow* parent);
	CUIMapList*			InitMapList(const char* path, CUIWindow* parent);
	CUIMapInfo*			InitMapInfo(const char* path, CUIWindow* parent);
	CUITrackBar*		InitTrackBar(const char* path, CUIWindow* parent);
	CUIArrowStepper*	InitArrowStepper(const char* path, CUIWindow* parent);
	CUIEditBox*			InitCDkey(const char* path, CUIWindow* parent);
	CUIEditBox*			InitMPPlayerName(const char* path, CUIWindow* parent);
	CUIMMShniaga*		InitMMShniaga(const char* path, CUIWindow* parent);
	CUIWindow*			InitKeyBinding(const char* path, CUIWindow* parent);
	CUIScrollView*		InitScrollView(const char* path, CUIWindow* parent);
	CUIListBox*			InitListBox(const char* path, CUIWindow* parent);
	CUIProgressBar*		InitProgressBar(const char* path, CUIWindow* parent);
	CUIArrow*			InitArrow(const char* path, CUIWindow* parent);
protected:
	CUIXml	m_xml;
public:
	DECLARE_SCRIPT_REGISTER_FUNCTION
};
