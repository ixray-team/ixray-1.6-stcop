////////////////////////////////////////////////////////////////////////////
//	Module 		: XR_IOConsole_callback.cpp
//	Created 	: 17.05.2008
//	Modified	: 21.07.2026
//	Author		: Evgeniy Sokolov
//	Description : Console`s callback functions class implementation
////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "line_editor.h"
#include "xr_ioc_cmd.h"

void CConsole::Register_callbacks()
{
	ec().assign_callback(SDL_SCANCODE_PAGEUP, text_editor::ks_free, Callback(this, &CConsole::Prev_log));
	ec().assign_callback(SDL_SCANCODE_PAGEDOWN, text_editor::ks_free, Callback(this, &CConsole::Next_log));
	ec().assign_callback(SDL_SCANCODE_PAGEUP, text_editor::ks_Ctrl, Callback(this, &CConsole::Begin_log));
	ec().assign_callback(SDL_SCANCODE_PAGEDOWN, text_editor::ks_Ctrl, Callback(this, &CConsole::End_log));

	ec().assign_callback(SDL_SCANCODE_TAB, text_editor::ks_free, Callback(this, &CConsole::Find_cmd));
	ec().assign_callback(SDL_SCANCODE_TAB, text_editor::ks_Shift, Callback(this, &CConsole::Find_cmd_back));

	ec().assign_callback(SDL_SCANCODE_UP, text_editor::ks_free, Callback(this, &CConsole::Prev_tip));
	ec().assign_callback(SDL_SCANCODE_DOWN, text_editor::ks_free, Callback(this, &CConsole::Next_tip));
	ec().assign_callback(SDL_SCANCODE_UP, text_editor::ks_Ctrl, Callback(this, &CConsole::Prev_cmd));
	ec().assign_callback(SDL_SCANCODE_DOWN, text_editor::ks_Ctrl, Callback(this, &CConsole::Next_cmd));

	ec().assign_callback(SDL_SCANCODE_HOME, text_editor::ks_Alt, Callback(this, &CConsole::Begin_tips));
	ec().assign_callback(SDL_SCANCODE_END, text_editor::ks_Alt, Callback(this, &CConsole::End_tips));
	ec().assign_callback(SDL_SCANCODE_PAGEUP, text_editor::ks_Alt, Callback(this, &CConsole::PageUp_tips));
	ec().assign_callback(SDL_SCANCODE_PAGEDOWN, text_editor::ks_Alt, Callback(this, &CConsole::PageDown_tips));

	ec().assign_callback(SDL_SCANCODE_RETURN, text_editor::ks_free, Callback(this, &CConsole::Execute_cmd));
	ec().assign_callback(SDL_SCANCODE_KP_ENTER, text_editor::ks_free, Callback(this, &CConsole::Execute_cmd));

	ec().assign_callback(SDL_SCANCODE_ESCAPE, text_editor::ks_free, Callback(this, &CConsole::Hide_cmd_esc));
	ec().assign_callback(SDL_SCANCODE_GRAVE, text_editor::ks_free, Callback(this, &CConsole::Hide_cmd));
	ec().assign_callback(SDL_SCANCODE_DELETE, text_editor::ks_Ctrl, Callback(this, &CConsole::Clear));
}

/**
 * @code SDL_SCANCODE_PAGEUP @endcode
 */
void CConsole::Prev_log()
{
	scroll_delta++;
	scroll_delta = std::min<int>(scroll_delta, (int)m_log_history.GetSize());

	// check for empty line
	xrCriticalSectionGuard guard(&m_log_history_guard);
	const shared_str& line = m_log_history.GetLooped(m_log_history.GetHead() - u32(scroll_delta) - 5u);
	if (line.size() == 0)
	{
		scroll_delta--;
	}
}

/**
 * @code SDL_SCANCODE_PAGEDOWN @endcode
 */
void CConsole::Next_log()
{
	scroll_delta--;
	scroll_delta = std::max(scroll_delta, 0);
}

/**
 * @code SDL_SCANCODE_PAGEUP + KMOD_CTRL @endcode
 */
void CConsole::Begin_log()
{
	scroll_delta = 0;
}

/**
 * @code SDL_SCANCODE_PAGEDOWN + KMOD_CTRL @endcode
 */
void CConsole::End_log()
{
	scroll_delta = 0;
}

/**
 * @code SDL_SCANCODE_TAB @endcode
 */
void CConsole::Find_cmd() 
{
	shared_str out_str;
		
	IConsole_Command* cc = find_next_cmd( ec().str_edit(), out_str );
	if (cc && out_str.size()) {
		ec().set_edit( out_str.c_str() );
	}
}

/**
 * @code SDL_SCANCODE_TAB + KMOD_SHIFT @endcode
 */
void CConsole::Find_cmd_back()
{
	const char* edt      = ec().str_edit();
	const char* radmin_cmd_name = "ra ";
	bool b_ra  = (edt == strstr( edt, radmin_cmd_name ) );
	u32 offset = (b_ra)? xr_strlen( radmin_cmd_name ) : 0;

	vecCMD_IT it = Commands.lower_bound( edt + offset );
	if ( it != Commands.begin() )
	{
		--it;
		IConsole_Command& cc = *(it->second);
		const char* name_cmd      = cc.Name();
		u32    name_cmd_size = xr_strlen( name_cmd );
		char*   new_str  = (char*)_alloca( (offset + name_cmd_size + 2) * sizeof(char) );

		xr_strcpy( new_str, offset + name_cmd_size + 2, (b_ra)? radmin_cmd_name : "" );
		xr_strcat( new_str, offset + name_cmd_size + 2, name_cmd );
		ec().set_edit( new_str );
	}
}

/**
 * @code SDL_SCANCODE_UP + KMOD_CTRL @endcode
 */
void CConsole::Prev_cmd() 
{
	prev_cmd_history_idx();
	SelectCommand();
}

/**
 * @code SDL_SCANCODE_DOWN + KMOD_CTRL @endcode
 */
void CConsole::Next_cmd() 
{
	next_cmd_history_idx();
	SelectCommand();
}

/**
 * @code SDL_SCANCODE_UP @endcode
 */
void CConsole::Prev_tip()
{
	if (xr_strlen(ec().str_edit()) == 0)
	{
		prev_cmd_history_idx();
		SelectCommand();
		return;
	}
	prev_selected_tip();
}

/**
 * @code SDL_SCANCODE_DOWN + KMOD_CTRL @endcode
 */
void CConsole::Next_tip()
{
	if (xr_strlen(ec().str_edit()) == 0)
	{
		next_cmd_history_idx();
		SelectCommand();
		return;
	}
	next_selected_tip();
}

void CConsole::Begin_tips() {
	m_select_tip = 0;
	m_start_tip = 0;
}

void CConsole::End_tips() {
	m_select_tip = (int)m_tips.size() - 1;
	m_start_tip = m_select_tip - VIEW_TIPS_COUNT + 1;
	check_next_selected_tip();
}

void CConsole::PageUp_tips() {
	m_select_tip -= VIEW_TIPS_COUNT;
	check_prev_selected_tip();
}

void CConsole::PageDown_tips() {
	m_select_tip += VIEW_TIPS_COUNT;
	check_next_selected_tip();
}

/**
 * @code SDL_SCANCODE_RETURN, SDL_SCANCODE_KP_ENTER @endcode
 */
void CConsole::Execute_cmd()
{
	if (0 <= m_select_tip && std::cmp_less(m_select_tip, m_tips.size()))
	{
		shared_str const& str = m_tips[m_select_tip].text;
		if (m_tips_mode == 1)
		{
			string512 buf = {};
			xr_strconcat(buf, str.c_str(), " ");
			ec().set_edit(buf);
		}
		else if (m_tips_mode == 2)
		{
			string512 buf = {};
			xr_strconcat(buf, m_cur_cmd.c_str(), " ", str.c_str());
			ec().set_edit(buf);
		}
		reset_selected_tip();
	}
	else
	{
		ExecuteCommand(ec().str_edit(), true, false);
	}
	m_disable_tips = false;
}

void CConsole::Show_cmd()
{
	Show();
}

void CConsole::Hide_cmd()
{
	Hide();
}

void CConsole::Hide_cmd_esc()
{
	if (0 <= m_select_tip && std::cmp_less(m_select_tip, m_tips.size()))
	{
		m_disable_tips = true;
		return;
	}
	Hide();
}