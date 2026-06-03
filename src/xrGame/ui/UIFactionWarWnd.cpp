////////////////////////////////////////////////////////////////////////////
//	Module 		: UIFactionWarWnd.cpp
//	Created 	: 26.12.2007
//	Author		: Evgeniy Sokolov
//	Description : UI Faction War window class implementation
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "pch_script.h"
#include "UIFactionWarWnd.h"
#include "../../xrEngine/xr_input.h"
#include "../../xrUI/UIXmlInit.h"
#include "../../xrUI/Widgets/UIProgressBar.h"
#include "../../xrUI/Widgets/UIFrameLineWnd.h"
#include "../../xrUI/UIHelper.h"
#include "../../xrUI/UICursor.h"
#include "FactionState.h"
#include "UIPdaWnd.h"
#include "UICharacterInfo.h"
#include "PdaConstants.h"
#include "PdaScriptBridge.h"

#include "../Actor.h"
#include "../ai_space.h"
#include "../../xrScripts/script_engine.h"

CUIFactionWarWnd::CUIFactionWarWnd()
{
	Reset();
	ActionRepeaters()->Register(this, kUI_LEFT);
	ActionRepeaters()->Register(this, kUI_RIGHT);
}

CUIFactionWarWnd::~CUIFactionWarWnd()
{
}

void CUIFactionWarWnd::Reset()
{
	m_previous_time    = Device.dwTimeGlobal;
	m_update_delay     = 3000;
	m_max_member_count = 100;
	m_max_resource     = 100;
	m_max_power        = 100;
	m_war_states_dx    = 0.0f;
	hint_wnd           = nullptr;
	m_tc_pos.set       ( 0.0f, 0.0f );
	m_td_pos.set       ( 0.0f, 0.0f );

	m_background = nullptr;
	m_center_background = nullptr;

	m_background2 = nullptr;
	m_center_background2 = nullptr;
}

void CUIFactionWarWnd::Init()
{
	CUIXml xml;
	xml.Load( CONFIG_PATH, UI_PATH, PdaXml::FactionWar );

	CUIXmlInit::InitWindow( xml, "main_wnd", 0, this );

    m_background = UIHelper::CreateFrameWindow(xml, "background", this, false);
    m_center_background = UIHelper::CreateFrameWindow(xml, "center_background", this, false);

    if (!m_background)
        m_background2 = UIHelper::CreateFrameLine(xml, "background", this, false);

    if (!m_center_background)
        m_center_background2 = UIHelper::CreateStatic(xml, "center_background", this);

	m_target_static			= UIHelper::CreateStatic( xml, "target_static", this );
	m_target_caption		= UIHelper::CreateStatic( xml, "target_caption", this );
	m_tc_pos				= m_target_caption->GetWndPos();

	m_target_desc			= UIHelper::CreateStatic( xml, "target_decs", this );
	m_td_pos				= m_target_desc->GetWndPos();

	m_state_static			= UIHelper::CreateStatic( xml, "state_static", this );
	
	m_our_icon				= UIHelper::CreateStatic( xml, "static_our_icon", this );
	m_our_icon_over			= UIHelper::CreateStatic( xml, "static_our_icon_over", this );
	m_our_name				= UIHelper::CreateStatic( xml, "static_our_name", this );
	m_st_our_frac_info		= UIHelper::CreateStatic( xml, "static_our_frac_info", this );
	m_st_our_mem_count		= UIHelper::CreateStatic( xml, "static_our_mem_count", this );
	m_st_our_resource		= UIHelper::CreateStatic( xml, "static_our_resource", this );
	
	m_pb_our_state			= UIHelper::CreateProgressBar( xml, "progress_our_state", this );
	m_pb_our_mem_count		= UIHelper::CreateProgressBar( xml, "progress_our_mem_count", this );
	m_pb_our_resource		= UIHelper::CreateProgressBar( xml, "progress_our_resource", this );

	m_enemy_icon			= UIHelper::CreateStatic( xml, "static_enemy_icon", this );
	m_enemy_icon_over		= UIHelper::CreateStatic( xml, "static_enemy_icon_over", this );
	m_enemy_name			= UIHelper::CreateStatic( xml, "static_enemy_name", this );
	m_st_enemy_frac_info	= UIHelper::CreateStatic( xml, "static_enemy_frac_info", this );
	m_st_enemy_mem_count	= UIHelper::CreateStatic( xml, "static_enemy_mem_count", this );
	m_st_enemy_resource		= UIHelper::CreateStatic( xml, "static_enemy_resource", this );

	m_pb_enemy_state		= UIHelper::CreateProgressBar( xml, "progress_enemy_state", this );
	m_pb_enemy_mem_count	= UIHelper::CreateProgressBar( xml, "progress_enemy_mem_count", this );	
	m_pb_enemy_resource		= UIHelper::CreateProgressBar( xml, "progress_enemy_resource", this );	

	m_static_line1			= UIHelper::CreateFrameLine( xml, "static_line1", this );
	m_static_line2			= UIHelper::CreateFrameLine( xml, "static_line2", this );
	m_static_line3			= UIHelper::CreateFrameLine( xml, "static_line3", this );
	m_static_line4			= UIHelper::CreateFrameLine( xml, "static_line4", this );
	m_static_line_left		= UIHelper::CreateFrameLine( xml, "static_line_left", this );
	m_static_line_right		= UIHelper::CreateFrameLine( xml, "static_line_right", this );

	VERIFY( hint_wnd );
	m_war_states_parent = new CUIWindow();
	m_war_states_parent->SetAutoDelete( true );
	AttachChild( m_war_states_parent );
	Fvector2 pos;
	pos.x = xml.ReadAttribFlt( "static_vs_state", 0, "x" );
	pos.y = xml.ReadAttribFlt( "static_vs_state", 0, "y" );
	m_war_states_parent->SetWndPos( pos );

	for ( u8 i = 0; i < max_war_state; ++i )
	{
		UIWarState* state = new UIWarState();
		state->InitXML(xml, "static_vs_state", m_war_states_parent);
		state->set_hint_wnd(hint_wnd);
		m_war_state.push_back(state);
	}
	
	float dx = xml.ReadAttribFlt( "static_vs_state", 0, "dx" );
	m_war_states_dx = dx;
	m_war_states_xcenter = xml.ReadAttribFlt( "static_vs_state", 0, "xcenter", 511.0f );

	pos.set( 0.0f, 0.0f );
	m_war_state[0]->SetWndPos( pos );
	for ( u8 i = 1; i < max_war_state; ++i )
	{
		pos.x += m_war_state[i-1]->GetWndSize().x + dx;
		m_war_state[i]->SetWndPos( pos );
	}

	for ( u8 i = 0; i < max_bonuce; ++i )
	{
		m_our_bonuces[i] = UIHelper::CreateStatic( xml, "static_our_bonuce", this );
	}
	dx = xml.ReadAttribFlt( "static_our_bonuce", 0, "dx" );
	pos = m_our_bonuces[0]->GetWndPos();
	for ( u8 i = 1; i < max_bonuce; ++i )
	{
		pos.x += m_our_bonuces[i-1]->GetWndSize().x + dx;
		m_our_bonuces[i]->SetWndPos( pos );
	}

	for ( u8 i = 0; i < max_bonuce; ++i )
	{
		m_enemy_bonuces[i] = UIHelper::CreateStatic( xml, "static_enemy_bonuce", this );
	}
	dx = xml.ReadAttribFlt( "static_enemy_bonuce", 0, "dx" );
	pos = m_enemy_bonuces[0]->GetWndPos();
	for ( u8 i = 1; i < max_bonuce; ++i )
	{
		pos.x += m_enemy_bonuces[i-1]->GetWndSize().x + dx;
		m_enemy_bonuces[i]->SetWndPos( pos );
	}
	int delay = xml.ReadAttribInt( "main_wnd", 0, "update_delay", 3000 );
	m_update_delay = (0 < delay)? (u32)delay : 0;
	m_gamepad_legend = UIHelper::CreateGamepadLegend(xml, "gamepad_legend", this, false);
}

void CUIFactionWarWnd::ShowInfo( bool status )
{
	m_state_static->Show( status );

	m_static_line2->Show( status );
	m_static_line3->Show( status );
	m_static_line4->Show( status );
	m_static_line_left->Show( status );
	m_static_line_right->Show( status );

	m_our_icon->Show( status );
	m_our_icon_over->Show( status );
	m_our_name->Show( status );
	m_st_our_frac_info->Show( status );
	m_st_our_mem_count->Show( status );
	m_st_our_resource->Show( status );

	m_pb_our_state->Show( status );
	m_pb_our_mem_count->Show( status );
	m_pb_our_resource->Show( status );

	m_enemy_icon->Show( status );
	m_enemy_icon_over->Show( status );
	m_enemy_name->Show( status );
	m_st_enemy_frac_info->Show( status );
	m_st_enemy_mem_count->Show( status );
	m_st_enemy_resource->Show( status );

	m_pb_enemy_state->Show( status );
	m_pb_enemy_mem_count->Show( status );
	m_pb_enemy_resource->Show( status );

	m_war_states_parent->Show( status );
	
	for ( u8 i = 0; i < max_bonuce; ++i )
	{
		m_our_bonuces[i]->Show( status );
		m_enemy_bonuces[i]->Show( status );
	}
}

void CUIFactionWarWnd::SendMessage( CUIWindow* pWnd, s16 msg, void* pData )
{
	CUIWndCallback::OnEvent( pWnd, msg, pData );
}

void CUIFactionWarWnd::Show( bool status )
{
	if ( status )
	{
		InitFactions();
	}
	for ( u8 i = 0; i < max_war_state; ++i )
	{
		m_war_state[i]->ClearInfo();
	}
	inherited::Show( status );
}

void CUIFactionWarWnd::Update()
{
	inherited::Update();
	if (CUIFrameWindow* frame = m_war_state[m_current_window]->m_frame_selected)
	{
		frame->Show(pInput->GetControllerMode());
	}
	if ( !IsShown() )
	{
		Reset();
	}	
	if ( Device.dwTimeGlobal - m_previous_time > m_update_delay )
	{
		m_previous_time = Device.dwTimeGlobal;
		UpdateInfo();
	}
}

bool CUIFactionWarWnd::InitFactions()
{
	shared_str our, enemy;
	if ( !CUICharacterInfo::get_actor_community( &our, &enemy ) )
	{
		return false;
	}
	
	m_our_faction.set_faction_id2( our );
	m_enemy_faction.set_faction_id2( enemy );

	UpdateInfo();
	return true;
}

void CUIFactionWarWnd::UpdateInfo()
{
	if ( m_our_faction.get_faction_id2().size() == 0 )
	{
		if ( !InitFactions() )
		{
			if (Device.IsEditorMode())
			{
				return;
			}
			else
			{
				R_ASSERT2(0, "Actor`s faction is unknown!");
			}
		}
	}
	m_max_member_count = get_max_member_count();
	m_max_resource     = get_max_resource();
	m_max_power        = get_max_power();
		
	m_our_faction.update_info();

	m_target_caption->SetText( m_our_faction.get_target() );
	m_target_caption->AdjustHeightToText();

	Fvector2 pos = m_td_pos;
	pos.y = m_target_caption->GetWndPos().y + m_target_caption->GetHeight() + 8.0f;
	m_target_desc->SetWndPos( pos );
	m_target_desc->SetText( m_our_faction.get_target_desc() );

	if ( m_enemy_faction.get_faction_id2().size() == 0 || m_our_faction.member_count == 0 || xr_strlen( m_our_faction.get_name() )==0 )
	{
		ShowInfo( false );
		return;
	}
	m_enemy_faction.update_info();
	ShowInfo( true );

	UpdateWarStates( m_our_faction );

	//our
	m_our_name->SetTextST(   m_our_faction.get_name() );
	m_our_icon->InitTexture( m_our_faction.get_icon_big() );


	m_pb_our_state->SetRange( 0.0f, m_max_power );
	m_pb_our_state->SetProgressPos( m_our_faction.power );

	m_pb_our_mem_count->SetRange( 0.0f, (float)m_max_member_count );
	m_pb_our_mem_count->SetProgressPos( (float)m_our_faction.member_count );

	m_pb_our_resource->SetRange( 0.0f, m_max_resource );
	m_pb_our_resource->SetProgressPos( m_our_faction.resource );
	set_amount_our_bonus( m_our_faction.bonus );

	//enemy
	m_enemy_name->SetTextST(   m_enemy_faction.get_name() );
	m_enemy_icon->InitTexture( m_enemy_faction.get_icon_big() );

	m_pb_enemy_state->SetRange( 0.0f, m_max_power );
	m_pb_enemy_state->SetProgressPos( m_enemy_faction.power );

	m_pb_enemy_mem_count->SetRange( 0.0f, (float)m_max_member_count );
	m_pb_enemy_mem_count->SetProgressPos( (float)m_enemy_faction.member_count );
	
	m_pb_enemy_resource->SetRange( 0.0f, m_max_resource );
	m_pb_enemy_resource->SetProgressPos(  m_enemy_faction.resource );
	
	set_amount_enemy_bonus( m_enemy_faction.bonus );
}

void CUIFactionWarWnd::UpdateWarStates( FactionState const& faction )
{
	Fvector2 pos;
	pos = m_war_states_parent->GetWndPos();

	float sx = 0.0f;
	m_factions_count = 0;
	for ( u8 i = 0; i < max_war_state; ++i )
	{
		if ( !m_war_state[i]->UpdateInfo( faction.get_war_state(i), faction.get_war_state_hint(i) ) )
		{
			break; // for i
		}
		++m_factions_count;
		sx += m_war_state[i]->GetWndSize().x + m_war_states_dx;
	}
	if ( m_factions_count == 0 )
	{
		m_war_states_parent->SetWndPos( pos );
		return;
	}
	sx -= m_war_states_dx;

	pos.x = m_war_states_xcenter - sx * 0.5f;
	m_war_states_parent->SetWndPos( pos );
}

void CUIFactionWarWnd::set_amount_our_bonus( int value )
{
	for ( u32 i = 0; i < max_bonuce; ++i )
	{
		m_our_bonuces[i]->TextItemControl()->SetTextColor( color_rgba( 255, 255, 255, 70) );
	}
	constexpr u32 cr = color_rgba( 0, 255, 0, 255);
	for ( int i = 0; i < value; ++i )
	{
		m_our_bonuces[i]->TextItemControl()->SetTextColor( cr );
	}
}

void CUIFactionWarWnd::set_amount_enemy_bonus( int value )
{
	for ( u32 i = 0; i < max_bonuce; ++i )
	{
		m_enemy_bonuces[i]->TextItemControl()->SetTextColor( color_rgba( 255, 255, 255, 70) );
	}
	constexpr u32 cr = color_rgba( 0, 255, 0, 255);
	for ( int i = 0; i < value; ++i )
	{
		m_enemy_bonuces[i]->TextItemControl()->SetTextColor( cr );
	}
}

// -------------------------------------------------------------------------------------------------
int CUIFactionWarWnd::get_max_member_count()
{
	int value = 100;
	PdaScriptBridge::TryCall(PdaScript::GetMaxMemberCount, value);
	return value;
}

float CUIFactionWarWnd::get_max_resource()
{
	float value = 100.0f;
	PdaScriptBridge::TryCall(PdaScript::GetMaxResource, value);
	return value;
}

float CUIFactionWarWnd::get_max_power()
{
	float value = 100.0f;
	PdaScriptBridge::TryCall(PdaScript::GetMaxPower, value);
	return value;
}

bool CUIFactionWarWnd::OnGamepadKeyAction(int id, EUIMessages gamepad_action)
{
	if (WINDOW_KEY_PRESSED == gamepad_action)
	{
		bool currentWindowShown = m_war_state[m_current_window]->IsShown();
		if (currentWindowShown)
		{
			switch (get_binded_action(id, agUIGeneral))
			{
				case kUI_LEFT:
					if (!any_binded_key_for_action_pressed_c(kUI_RIGHT))
					{
						TurnLeft(true);
					}
					ActionRepeaters()->SetActionStarted(this, kUI_LEFT);
					return true;
				case kUI_RIGHT:
					if (!any_binded_key_for_action_pressed_c(kUI_LEFT))
					{
						TurnRight(true);
					}
					ActionRepeaters()->SetActionStarted(this, kUI_RIGHT);
					return true;
				case kUI_HINT:
					Fvector2 pos;
					m_war_state[m_current_window]->GetAbsolutePos(pos);
					pos.add(m_war_state[m_current_window]->GetWndSize());
					UI().GetUICursor().SetUICursorPosition(pos);

					m_war_state[m_current_window]->ToggleHint();
					return true;
			}
		}
	}

	return inherited::OnGamepadKeyAction(id, gamepad_action);
}

bool CUIFactionWarWnd::OnGamepadKeyHold(int id)
{
	switch (get_binded_action(id, agUIGeneral))
	{
		case kUI_LEFT:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kUI_LEFT) && !any_binded_key_for_action_pressed_c(kUI_RIGHT))
			{
				TurnLeft();
			}
			return true;
		}
		case kUI_RIGHT:
		{
			if (ActionRepeaters()->CanRepeatActionNow(this, kUI_RIGHT) && !any_binded_key_for_action_pressed_c(kUI_LEFT))
			{
				TurnRight();
			}
			return true;
		}
	}
	return inherited::OnGamepadKeyHold(id);
}

void CUIFactionWarWnd::TurnLeft(bool loop)
{
	bool showHint = m_war_state[m_current_window]->get_hint_wnd()->is_visible();
	u8 nextWindow = m_current_window - 1;
	if (nextWindow == u8(-1))
	{
		if (loop)
		{
			nextWindow = m_factions_count - 1;
		}
		else
		{
			return;
		}
	}

	if (showHint)
	{
		m_war_state[m_current_window]->ToggleHint();
	}
	if (CUIFrameWindow* frame = m_war_state[m_current_window]->m_frame_selected)
	{
		frame->Show(false);
	}
	m_current_window = nextWindow;
	if (showHint)
	{
		Fvector2 pos;
		m_war_state[m_current_window]->GetAbsolutePos(pos);
		pos.add(m_war_state[m_current_window]->GetWndSize());
		UI().GetUICursor().SetUICursorPosition(pos);

		m_war_state[m_current_window]->ToggleHint();
	}
	if (CUIFrameWindow* frame = m_war_state[m_current_window]->m_frame_selected)
	{
		frame->Show(true);
	}
}

void CUIFactionWarWnd::TurnRight(bool loop)
{
	bool showHint = m_war_state[m_current_window]->get_hint_wnd()->is_visible();
	u8 nextWindow = m_current_window + 1;
	if (nextWindow >= m_factions_count)
	{
		if (loop)
		{
			nextWindow = 0;
		}
		else
		{
			return;
		}
	}

	if (showHint)
	{
		m_war_state[m_current_window]->ToggleHint();
	}
	if (CUIFrameWindow* frame = m_war_state[m_current_window]->m_frame_selected)
	{
		frame->Show(false);
	}
	m_current_window = nextWindow;
	if (showHint)
	{
		Fvector2 pos;
		m_war_state[m_current_window]->GetAbsolutePos(pos);
		pos.add(m_war_state[m_current_window]->GetWndSize());
		UI().GetUICursor().SetUICursorPosition(pos);

		m_war_state[m_current_window]->ToggleHint();
	}
	if (CUIFrameWindow* frame = m_war_state[m_current_window]->m_frame_selected)
	{
		frame->Show(true);
	}
}