////////////////////////////////////////////////////////////////////////////
//	Module 		: UIHelper.cpp
//	Created 	: 17.01.2008
//	Author		: Evgeniy Sokolov
//	Description : UI Helper class implementation
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "UIHelper.h"
#include "UIXmlInit.h"

#include "Widgets/UIProgressBar.h"
#include "Widgets/UIProgressShape.h"
#include "Widgets/UIFrameLineWnd.h"
#include "Widgets/UIFrameWindow.h"
#include "Widgets/UI3tButton.h"
#include "Widgets/UICheckButton.h"
#include "Widgets/UIHint.h"
#include "Widgets/UIEditBox.h"
#include "Widgets/UITrackBar.h"
#include "Widgets/UIScrollView.h"

CUIStatic* UIHelper::CreateStatic( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical )
{
    // If it's not critical element, then don't crash if it doesn't exist
    if (!critical && !xml.NavigateToNode(ui_path, 0))
        return nullptr;

    auto ui = new CUIStatic();
    if (!CUIXmlInit::InitStatic(xml, ui_path, 0, ui, critical))
    {
        R_ASSERT4(!critical, "Failed to create static", ui_path, xml.m_xml_file_name);
        xr_delete(ui);
    }
    else if (parent)
    {
        parent->AttachChild(ui);
        ui->SetAutoDelete(true);
    }
    return ui;
}

CUIStackPanel* UIHelper::CreateStackPanel(CUIXml& xml, LPCSTR ui_path, CUIWindow* parent)
{
	CUIStackPanel* ui = new CUIStackPanel;
	if (parent)
	{
		parent->AttachChild(ui);
		ui->SetAutoDelete(true);
	}
	CUIXmlInit::InitStackPanel(xml, ui_path, 0, ui);
	return ui;
}

CUIEditBox* UIHelper::CreateEditBox( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent )
{
	CUIEditBox* ui			= new CUIEditBox();
	if(parent)
	{
		parent->AttachChild	( ui );
		ui->SetAutoDelete	( true );
	}
	CUIXmlInit::InitEditBox	( xml, ui_path, 0, ui );
	return ui;
}

CUIProgressBar* UIHelper::CreateProgressBar( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent )
{
	CUIProgressBar* ui			= new CUIProgressBar();
	parent->AttachChild			( ui );
	ui->SetAutoDelete			( true );
	CUIXmlInit::InitProgressBar ( xml, ui_path, 0, ui );
	return ui;
}

CUIProgressShape* UIHelper::CreateProgressShape(CUIXml& xml, LPCSTR ui_path, CUIWindow* parent)
{
	CUIProgressShape* ui = new CUIProgressShape();
	parent->AttachChild(ui);
	ui->SetAutoDelete(true);
	CUIXmlInit::InitProgressShape(xml, ui_path, 0, ui);
	return ui;
}

CUIFrameLineWnd* UIHelper::CreateFrameLine(CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical)
{
    // If it's not critical element, then don't crash if it doesn't exist
    if (!critical && !xml.NavigateToNode(ui_path, 0))
        return nullptr;

    auto ui = new CUIFrameLineWnd();
    if (!CUIXmlInit::InitFrameLine(xml, ui_path, 0, ui, critical))
    {
        R_ASSERT4(!critical, "Failed to create frame line", ui_path, xml.m_xml_file_name);
        xr_delete(ui);
    }
    else if (parent)
    {
        parent->AttachChild(ui);
        ui->SetAutoDelete(true);
    }
    return ui;
}

CUIFrameWindow* UIHelper::CreateFrameWindow(CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical)
{
	// If it's not critical element, then don't crash if it doesn't exist
	if (!critical && !xml.NavigateToNode(ui_path, 0))
		return nullptr;

	auto ui = new CUIFrameWindow();
	if (!CUIXmlInit::InitFrameWindow(xml, ui_path, 0, ui, critical))
	{
		R_ASSERT4(!critical, "Failed to create frame window", ui_path, xml.m_xml_file_name);
		xr_delete(ui);
	}
	else if (parent)
	{
		parent->AttachChild(ui);
		ui->SetAutoDelete(true);
	}
	return ui;
}

CUI3tButton* UIHelper::Create3tButton( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent )
{
	CUI3tButton* ui				= new CUI3tButton();
	parent->AttachChild			( ui );
	ui->SetAutoDelete			( true );
	CUIXmlInit::Init3tButton	( xml, ui_path, 0, ui );
	return ui;
}

CUICheckButton* UIHelper::CreateCheck( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent )
{
	CUICheckButton* ui			= new CUICheckButton();
	parent->AttachChild			( ui );
	ui->SetAutoDelete			( true );
	CUIXmlInit::InitCheck		( xml, ui_path, 0, ui );
	return ui;
}

UIHint* UIHelper::CreateHint( CUIXml& xml, LPCSTR ui_path)
{
	UIHint* ui					= new UIHint();
	ui->SetAutoDelete			( true );
	ui->init_from_xml			( xml, ui_path );
	return ui;
}

CUITrackBar* UIHelper::CreateTrackBar( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent )
{
	CUITrackBar* ui			= new CUITrackBar();
	if(parent)
	{
		parent->AttachChild	( ui );
		ui->SetAutoDelete	( true );
	}
	CUIXmlInit::InitTrackBar( xml, ui_path, 0, ui );
	return ui;
}

CUIScrollView* UIHelper::CreateScrollView( CUIXml& xml, LPCSTR ui_path, CUIWindow* parent, bool critical )
{
    // If it's not critical element, then don't crash if it doesn't exist
    if (!critical && !xml.NavigateToNode(ui_path, 0))
        return nullptr;

    auto ui = new CUIScrollView();
    if (!CUIXmlInit::InitScrollView(xml, ui_path, 0, ui, critical))
    {
        R_ASSERT4(!critical, "Failed to create static", ui_path, xml.m_xml_file_name);
        xr_delete(ui);
    }
    else if (parent)
    {
        parent->AttachChild(ui);
        ui->SetAutoDelete(true);
    }
    return ui;
}
