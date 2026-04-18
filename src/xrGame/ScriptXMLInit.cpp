#include "StdAfx.h"
#include "pch_script.h"
#include "ScriptXMLInit.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UITextureMaster.h"
#include "../../xrUI/Widgets/UICheckButton.h"
#include "../../xrUI/Widgets/UISpinNum.h"
#include "../../xrUI/Widgets/UISpinText.h"
#include "../../xrUI/Widgets/UIComboBox.h"
#include "../../xrUI/Widgets/UITabControl.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"
#include "ui/ServerList.h"
#include "ui/UIMapList.h"
#include "ui/UIKeyBinding.h"
#include "../../xrUI/Widgets/UIEditBox.h"
#include "../../xrUI/Widgets/UIAnimatedStatic.h"
#include "../../xrUI/Widgets/UITrackBar.h"
#include "../../xrUI/Widgets/UIArrowStepper.h"
#include "ui/UICDkey.h"
#include "ui/UIMapInfo.h"
#include "ui/UIMMShniaga.h"
#include "../../xrUI/Widgets/UIScrollView.h"
#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/Widgets/UIListWnd.h"
#include "../../xrUI/Widgets/UIStackPanel.h"
#include "../../xrUI/Widgets/UIArrow.h"
#include "../../xrUI/Widgets/UIGamepadLegend.h"

using namespace luabind;

void _attach_child(CUIWindow* _child, CUIWindow* _parent)
{
	if(!_parent)					return;

	_child->SetAutoDelete			(true);
	CUIScrollView* _parent_scroll	= smart_cast<CUIScrollView*>(_parent);
	if(_parent_scroll)
		_parent_scroll->AddWindow	(_child, true);
	else
		_parent->AttachChild		(_child);
}

void CScriptXmlInit::ParseFile(const char* xml_file)
{
	m_xml.Load(CONFIG_PATH, UI_PATH, xml_file);
}

void CScriptXmlInit::ParseDirFile(const char* xml_dir, const char* xml_file)
{
	m_xml.Load(CONFIG_PATH, xml_dir, xml_file);
}

bool CScriptXmlInit::NodeExist(const char* path, int index)
{
	if (m_xml.NavigateToNode(path, index))
	{
		return true;
	}
	return false;
}

int CScriptXmlInit::GetNodesNum(const char* path, int index, const char* tag_name)
{
	return m_xml.GetNodesNum(path, index, tag_name);
}

bool CScriptXmlInit::NavigateToNode(const char* path, int index)
{
	XML_NODE* node = m_xml.NavigateToNode(path, index);
	if (node)
	{
		m_xml.SetLocalRoot(node);
		return true;
	}
	return false;
}

bool CScriptXmlInit::NavigateToNode_ByAttribute(const char* tag_name, const char* attrib_name, const char* attrib_value)
{
	XML_NODE* node = m_xml.NavigateToNodeWithAttribute(tag_name, attrib_name, attrib_value);
	if (node)
	{
		m_xml.SetLocalRoot(node);
		return true;
	}
	return false;
}

bool CScriptXmlInit::NavigateToNode_ByPath(const char* path, int index, const char* tag_name, const char* attrib,
	const char* attrib_value_pattern)
{
	XML_NODE* node = m_xml.SearchForAttribute(path, index, tag_name, attrib, attrib_value_pattern);
	if (node)
	{
		m_xml.SetLocalRoot(node);
		return true;
	}
	return false;
}

void CScriptXmlInit::NavigateToRoot()
{
	m_xml.SetLocalRoot(m_xml.GetRoot());
}

const char* CScriptXmlInit::ReadValue(const char* path, int index)
{
	return m_xml.Read(path, index, "");
}

const char* CScriptXmlInit::ReadAttribute(const char* path, int index, const char* attrib)
{
	return m_xml.ReadAttrib(path, index, attrib, "");
}

u32 CScriptXmlInit::GetColor(const char* path, int index)
{
	return CUIXmlInit::GetColor(m_xml, path, index, 0xFFFFFFFF);
}

void CScriptXmlInit::InitWindow(const char* path, int index, CUIWindow* pWnd)
{
	CUIXmlInit::InitWindow(m_xml, path, index, pWnd);
}


CUIFrameWindow*	CScriptXmlInit::InitFrame(const char* path, CUIWindow* parent)
{
	CUIFrameWindow* pWnd = new CUIFrameWindow();
	CUIXmlInit::InitFrameWindow(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}


CUIFrameLineWnd* CScriptXmlInit::InitFrameLine(const char* path, CUIWindow* parent)
{
	CUIFrameLineWnd* pWnd = new CUIFrameLineWnd();
	CUIXmlInit::InitFrameLine(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}


CUIEditBox* CScriptXmlInit::InitEditBox(const char* path, CUIWindow* parent)
{
	CUIEditBox* pWnd = new CUIEditBox();
	CUIXmlInit::InitEditBox(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUIStatic* CScriptXmlInit::InitStatic(const char* path, CUIWindow* parent)
{
	CUIStatic* pWnd = new CUIStatic();
	CUIXmlInit::InitStatic(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUIStackPanel* CScriptXmlInit::InitStackPanel(const char* path, CUIWindow* parent)
{
	CUIStackPanel* pWnd = new CUIStackPanel();
	CUIXmlInit::InitStackPanel(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUIGamepadLegend* CScriptXmlInit::InitGamepadLegend(const char* path, CUIWindow* parent)
{
	CUIGamepadLegend* pWnd = new CUIGamepadLegend();
	CUIXmlInit::InitGamepadLegend(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUIStatic* CScriptXmlInit::InitAnimStatic(const char* path, CUIWindow* parent)
{
	CUIAnimatedStatic* pWnd = new CUIAnimatedStatic();
	CUIXmlInit::InitAnimatedStatic(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUIStatic* CScriptXmlInit::InitSleepStatic(const char* path, CUIWindow* parent)
{
	CUISleepStatic* pWnd = new CUISleepStatic();
	CUIXmlInit::InitSleepStatic(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUIScrollView* CScriptXmlInit::InitScrollView(const char* path, CUIWindow* parent)
{
	CUIScrollView* pWnd = new CUIScrollView();
	CUIXmlInit::InitScrollView(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUIListWnd* CScriptXmlInit::InitListWnd(const char* path, CUIWindow* parent)
{
	CUIListWnd* pWnd = new CUIListWnd();
	CUIXmlInit::InitListWnd(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUIListBox*	CScriptXmlInit::InitListBox(const char* path, CUIWindow* parent)
{
	CUIListBox* pWnd = new CUIListBox();
	CUIXmlInit::InitListBox(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUICheckButton* CScriptXmlInit::InitCheck(const char* path, CUIWindow* parent)
{
	CUICheckButton* pWnd = new CUICheckButton();
	CUIXmlInit::InitCheck(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUISpinNum* CScriptXmlInit::InitSpinNum(const char* path, CUIWindow* parent)
{
	CUISpinNum* pWnd = new CUISpinNum();
	CUIXmlInit::InitSpin(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUISpinFlt* CScriptXmlInit::InitSpinFlt(const char* path, CUIWindow* parent)
{
	CUISpinFlt* pWnd = new CUISpinFlt();
	CUIXmlInit::InitSpin(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUISpinText* CScriptXmlInit::InitSpinText(const char* path, CUIWindow* parent)
{
	CUISpinText* pWnd = new CUISpinText();
	CUIXmlInit::InitSpin(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUIComboBox* CScriptXmlInit::InitComboBox(const char* path, CUIWindow* parent)
{
	CUIComboBox* pWnd = new CUIComboBox();
	CUIXmlInit::InitComboBox(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;
}

CUI3tButton* CScriptXmlInit::Init3tButton(const char* path, CUIWindow* parent)
{
	CUI3tButton* pWnd = new CUI3tButton();
	CUIXmlInit::Init3tButton(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;	
}

CUITabControl* CScriptXmlInit::InitTab(const char* path, CUIWindow* parent)
{
	CUITabControl* pWnd = new CUITabControl();
	CUIXmlInit::InitTabControl(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return pWnd;	
}


CServerList* CScriptXmlInit::InitServerList(const char* path, CUIWindow* parent)
{
	CServerList* pWnd = new CServerList();
	pWnd->InitFromXml(m_xml, path);	
	_attach_child(pWnd, parent);
	return pWnd;	
}

CUIMapList* CScriptXmlInit::InitMapList(const char* path, CUIWindow* parent)
{
	CUIMapList* pWnd = new CUIMapList();
	pWnd->InitFromXml(m_xml, path);	
	_attach_child(pWnd, parent);
	return pWnd;	
}

CUIMMShniaga* CScriptXmlInit::InitMMShniaga(const char* path, CUIWindow* parent)
{
	CUIMMShniaga* pWnd	= new CUIMMShniaga();
	pWnd->InitShniaga	(m_xml, path);
	_attach_child		(pWnd, parent);
	return pWnd;
}

CUIMapInfo* CScriptXmlInit::InitMapInfo(const char* path, CUIWindow* parent)
{
	CUIMapInfo* pWnd	= new CUIMapInfo();
	CUIXmlInit::InitWindow(m_xml,path,0,pWnd);
	pWnd->InitMapInfo(pWnd->GetWndPos(),pWnd->GetWndSize());
	_attach_child		(pWnd, parent);
	return pWnd;	
}

CUIWindow* CScriptXmlInit::InitKeyBinding(const char* path, CUIWindow* parent)
{
	CUIKeyBinding* pWnd				= new CUIKeyBinding();
	pWnd->InitFromXml				(m_xml, path);	
	_attach_child					(pWnd, parent);
	return							pWnd;
}

CUITrackBar* CScriptXmlInit::InitTrackBar(const char* path, CUIWindow* parent)
{
	CUITrackBar* pWnd				= new CUITrackBar();
	CUIXmlInit::InitTrackBar		(m_xml, path, 0, pWnd);
	_attach_child					(pWnd, parent);
	return							pWnd;	
}

CUIArrowStepper* CScriptXmlInit::InitArrowStepper(const char* path, CUIWindow* parent)
{
	CUIArrowStepper* pWnd = new CUIArrowStepper();
	CUIXmlInit::InitArrowStepper(m_xml, path, 0, pWnd);
	_attach_child(pWnd, parent);
	return							pWnd;
}

CUIProgressBar* CScriptXmlInit::InitProgressBar(const char* path, CUIWindow* parent)
{
	CUIProgressBar* pWnd			= new CUIProgressBar();
	CUIXmlInit::InitProgressBar		(m_xml, path, 0, pWnd);
	_attach_child					(pWnd, parent);
	return							pWnd;	
}

CUIEditBox* CScriptXmlInit::InitCDkey(const char* path, CUIWindow* parent)
{
	CUICDkey* pWnd					= new CUICDkey();
	CUIXmlInit::InitEditBox			(m_xml, path, 0, pWnd);
	pWnd->assign_callbacks			( );
	_attach_child					(pWnd, parent);
	pWnd->SetCurrentOptValue		();
	return							pWnd;	
}

CUIEditBox* CScriptXmlInit::InitMPPlayerName(const char* path, CUIWindow* parent)
{
	CUIMPPlayerName* pWnd			= new CUIMPPlayerName();
	CUIXmlInit::InitEditBox			(m_xml, path, 0, pWnd);
	_attach_child					(pWnd, parent);
	return							pWnd;	
}

CUIArrow* CScriptXmlInit::InitArrow(const char* path, CUIWindow* parent)
{
	CUIArrow* pWnd					= new CUIArrow();
	pWnd->init_from_xml				(m_xml, path, parent);
	return							pWnd;	
}

#pragma optimize("s",on)
void CScriptXmlInit::script_register(lua_State *L){
	module(L)
	[
		class_<CScriptXmlInit>			("CScriptXmlInit")
		.def(							constructor<>())
		.def("ParseFile",				&CScriptXmlInit::ParseFile)
		.def("ParseDirFile",			&CScriptXmlInit::ParseDirFile)

		.def("NodeExist",				&CScriptXmlInit::NodeExist)
		.def("GetNodesNum",				&CScriptXmlInit::GetNodesNum)
		.def("NavigateToNode",			&CScriptXmlInit::NavigateToNode)
		.def("NavigateToNode_ByAttribute", &CScriptXmlInit::NavigateToNode_ByAttribute)
		.def("NavigateToNode_ByPath",	&CScriptXmlInit::NavigateToNode_ByPath)
		.def("NavigateToRoot",			&CScriptXmlInit::NavigateToRoot)
		.def("ReadValue",				&CScriptXmlInit::ReadValue)
		.def("ReadAttribute",			&CScriptXmlInit::ReadAttribute)
		.def("GetColor",				&CScriptXmlInit::GetColor)

		.def("InitWindow",				&CScriptXmlInit::InitWindow)
		.def("InitFrame",				&CScriptXmlInit::InitFrame)
		.def("InitFrameLine",			&CScriptXmlInit::InitFrameLine)
		.def("InitLabel",				&CScriptXmlInit::InitFrameLine)
		.def("InitEditBox",				&CScriptXmlInit::InitEditBox)
		.def("InitStatic",				&CScriptXmlInit::InitStatic)
		.def("InitStackPanel",			&CScriptXmlInit::InitStackPanel)
		.def("InitGamepadLegend",		&CScriptXmlInit::InitGamepadLegend)
		.def("InitTextWnd",				&CScriptXmlInit::InitStatic)
		.def("InitAnimStatic",			&CScriptXmlInit::InitAnimStatic)
		.def("InitSleepStatic",			&CScriptXmlInit::InitSleepStatic)
		.def("Init3tButton",			&CScriptXmlInit::Init3tButton)
		.def("InitCheck",				&CScriptXmlInit::InitCheck)
		.def("InitSpinNum",				&CScriptXmlInit::InitSpinNum)
		.def("InitSpinFlt",				&CScriptXmlInit::InitSpinFlt)
		.def("InitSpinText",			&CScriptXmlInit::InitSpinText)
		.def("InitComboBox",			&CScriptXmlInit::InitComboBox)
		.def("InitTab",					&CScriptXmlInit::InitTab)
		.def("InitServerList",			&CScriptXmlInit::InitServerList)
		.def("InitMapList",				&CScriptXmlInit::InitMapList)
		.def("InitMapInfo",				&CScriptXmlInit::InitMapInfo)
		.def("InitTrackBar",			&CScriptXmlInit::InitTrackBar)
		.def("InitArrowStepper",		&CScriptXmlInit::InitArrowStepper)
		.def("InitCDkey",				&CScriptXmlInit::InitCDkey)
		.def("InitMPPlayerName",		&CScriptXmlInit::InitMPPlayerName)
		.def("InitKeyBinding",			&CScriptXmlInit::InitKeyBinding)
		.def("InitMMShniaga",			&CScriptXmlInit::InitMMShniaga)
		.def("InitScrollView",			&CScriptXmlInit::InitScrollView)
		.def("InitListBox",				&CScriptXmlInit::InitListBox)
		.def("InitList",				&CScriptXmlInit::InitListWnd)
		.def("InitProgressBar",			&CScriptXmlInit::InitProgressBar)
		.def("InitArrow",				&CScriptXmlInit::InitArrow)
	];

}
