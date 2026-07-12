////////////////////////////////////////////////////////////////////////////
//	Module 		: UIWarState.cpp
//	Created 	: 15.04.2008
//	Author		: Evgeniy Sokolov
//	Description : UI war state (PDA) window class implementation
////////////////////////////////////////////////////////////////////////////

#include "StdAfx.h"
#include "../../xrEngine/xr_input.h"
#include "UIWarState.h"
#include "../../xrUI/Widgets/UIStatic.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/Widgets/UIFrameWindow.h"

void UIWarState::InitXML( CUIXml& xml, const char* att_name, CUIWindow* parent )
{
	VERIFY( parent );
	parent->AttachChild( this );
	SetAutoDelete( true );
	string256 buf;	

	CUIXmlInit::InitWindow( xml, att_name, 0, this );

	xr_strconcat(buf, att_name, ":img" );
	m_static = UIHelper::CreateStatic( xml, buf, this );
	
	set_hint_delay( (u32)xml.ReadAttribInt( att_name, 0, "delay", 0 ) );
	
	m_frame_selected = new CUIFrameWindow();
	if (m_frame_selected->InitTexture("ui_inv_item_selector", false))
	{
		m_frame_selected->SetWidth(GetWidth());
		m_frame_selected->SetHeight(GetHeight());
		m_frame_selected->Show(false);
		AttachChild(m_frame_selected);
	}
	else
	{
		xr_delete(m_frame_selected);
	}
}

void UIWarState::ClearInfo()
{
	SetVisible( false );
	set_hint_text_ST( "" );
}

bool UIWarState::UpdateInfo( const char* icon, const char* hint_text )
{
	if ( !icon || !xr_strlen(icon) )
	{
		return false;
	}

	SetVisible( true );
	m_static->InitTexture( icon );

	if ( !hint_text || !xr_strlen(hint_text) )
	{
		set_hint_text_ST( "" );
	}
	else
	{
		set_hint_text_ST( hint_text );
	}
	return true;
}

void UIWarState::OnFocusReceive()
{
	if (!pInput->GetControllerMode())
	{
		inherited::OnFocusReceive();
	}
}

void UIWarState::ToggleHint()
{
	if (m_hint_wnd->is_visible())
	{
		m_hint_wnd->set_text(nullptr);
		m_enable = false;
	}
	else
	{
		m_hint_wnd->set_text(m_hint_text.c_str());
		m_enable = true;
	}
}