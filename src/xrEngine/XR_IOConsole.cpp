// XR_IOConsole.cpp: implementation of the CConsole class.
// modify 15.05.2008 sea

#include "stdafx.h"

#include "line_editor.h"

#include "IGame_Level.h"
#include "IGame_Persistent.h"

#include "x_ray.h"
#include "xr_ioc_cmd.h"
#include "GameFont.h"

#include "../Include/xrRender/UIRender.h"

#ifdef DEBUG_DRAW
#	include <imgui_internal.h>
#endif

constexpr float UI_BASE_HEIGHT = 768.0f;
constexpr u32 cmd_history_max = 64;

constexpr u32 prompt_font_color = color_rgba(228, 228, 255, 255);
constexpr u32 tips_font_color = color_rgba(230, 250, 230, 255);
constexpr u32 cmd_font_color = color_rgba(138, 138, 245, 255);
constexpr u32 cursor_font_color = color_rgba(255, 255, 255, 255);
constexpr u32 total_font_color = color_rgba(250, 250, 15, 180);
constexpr u32 default_font_color = color_rgba(250, 250, 250, 250);

constexpr u32 back_color = color_rgba(20, 20, 20, 200);
constexpr u32 tips_back_color = color_rgba(20, 20, 20, 200);
constexpr u32 tips_select_color = color_rgba(90, 90, 140, 230);
constexpr u32 tips_word_color = color_rgba(5, 100, 56, 200);
constexpr u32 tips_scroll_back_color = color_rgba(15, 15, 15, 230);
constexpr u32 tips_scroll_pos_color = color_rgba(70, 70, 70, 240);

constexpr const char* m_fontConsoleName = "ui_font_console";
constexpr const char* m_fontConsole2Name = "ui_font_console_2";


ENGINE_API CConsole* Console = nullptr;

extern char const* const ioc_prompt;
char const* const ioc_prompt = ">>> ";

extern char const* const ch_cursor;
char const* const ch_cursor = "_";

text_editor::line_edit_control& CConsole::ec()
{
	return m_editor->control();
}

u32 CConsole::get_mark_color(Console_mark type)
{
	switch (type)
	{
		case mark0:
			return color_rgba(255, 255, 0, 255);
		case mark1:
			return color_rgba(255, 0, 0, 255);
		case mark2:
			return color_rgba(100, 100, 255, 255);
		case mark3:
			return color_rgba(0, 222, 205, 155);
		case mark4:
			return color_rgba(255, 0, 255, 255);
		case mark5:
			return color_rgba(155, 55, 170, 155);
		case mark6:
			return color_rgba(25, 200, 50, 255);
		case mark7:
			return color_rgba(255, 255, 0, 255);
		case mark8:
			return color_rgba(128, 128, 128, 255);
		case mark9:
			return color_rgba(0, 255, 0, 255);
		case mark10:
			return color_rgba(55, 155, 140, 255);
		case mark11:
			return color_rgba(205, 205, 105, 255);
		case mark12:
			return color_rgba(128, 128, 250, 255);
	}
	return default_font_color;
}

bool CConsole::is_mark(Console_mark type)
{
	switch (type)
	{
		case mark0:
		case mark1:
		case mark2:
		case mark3:
		case mark4:
		case mark5:
		case mark6:
		case mark7:
		case mark8:
		case mark9:
		case mark10:
		case mark11:
		case mark12:
			return true;
	}
	return false;
}

void ConsoleLogCallback(const char* line) {
	Console->AddLogEntry(line);
}

CConsole::CConsole() : m_hShader_back(nullptr)
{
	m_editor = new text_editor::line_editor((u32)CONSOLE_BUF_SIZE);
	m_cmd_history_max = cmd_history_max;
	m_disable_tips = false;
	Register_callbacks();
	xrLogger::AddLogCallback(ConsoleLogCallback);
}

#ifdef DEBUG_DRAW
void CConsole::RegisterImGuiConsoleSettingsHandler()
{
	ImGuiSettingsHandler h = {};
	h.TypeName = "DebugConsoleVars";
	h.TypeHash = ImHashStr("DebugConsoleVars");
	h.ReadOpenFn = ImGuiReadOpenUIConsoleVars;
	h.ReadLineFn = ImGuiReadLineUIConsoleVars;
	h.WriteAllFn = ImGuiWriteAllUIConsoleVars;
	ImGui::AddSettingsHandler(&h);
	
	DebugConsoleVarsSettingsHandler = ImGui::FindSettingsHandler("DebugConsoleVars");
}
#endif

void CConsole::Initialize()
{
	scroll_delta = 0;
	bVisible = false;

	m_last_cmd = nullptr;

	m_cmd_history.reserve(m_cmd_history_max + 2);
	m_cmd_history.clear();
	ReadLastCmds();
	reset_cmd_history_idx();

	m_tips.reserve(MAX_TIPS_COUNT + 1);
	m_tips.clear();
	m_temp_tips.reserve(MAX_TIPS_COUNT + 1);
	m_temp_tips.clear();

	m_tips_mode = 0;
	m_prev_length_str = 0;
	m_cur_cmd = nullptr;

	reset_selected_tip();

	// Commands
	extern void CCC_Register();
	CCC_Register();

#ifdef DEBUG_DRAW
	if (!Device.IsEditorMode())
	{
		CImGuiManager::Instance().Subscribe("DebugConsole", CImGuiManager::ERenderPriority::eMedium, [this]
		{
			ImGuiDrawUIConsole();
		});

		CImGuiManager::Instance().Subscribe("DebugConsoleVars", CImGuiManager::ERenderPriority::eMedium, [this]
		{
			ImGuiDrawUIConsoleVars();
		});
	}
#endif
}

CConsole::~CConsole()
{
	xrLogger::RemoveLogCallback(ConsoleLogCallback);
	if (!Device.IsEditorMode())
	{
		CImGuiManager::Instance().Unsubscribe("DebugConsole");
		CImGuiManager::Instance().Unsubscribe("DebugConsoleVars");
	}

	xr_delete( m_hShader_back );
	xr_delete( m_editor );
	Destroy();
}

void CConsole::Destroy()
{
	FlushLastCmds();
	Commands.clear();
}

void CConsole::FlushLastCmds()
{
	if (m_cmd_history.empty())
	{
		return;
	}

	string_path path;
	FS.update_path(path, _app_data_root_, m_config_name);

	IWriter* writer = FS.w_open(path);

	xr_set<shared_str> filter;
	constexpr size_t HISTORY_SIZE = 15;

	size_t read_from = m_cmd_history.size() > HISTORY_SIZE ? m_cmd_history.size() - HISTORY_SIZE : 0;
	size_t read_to = m_cmd_history.size();

	for (size_t i = read_from; i < read_to; ++i)
	{
		if (shared_str& line = m_cmd_history[i]; !filter.contains(line))
		{
			filter.insert(line);

			string1024 buffer;
			_GetItem(line.c_str(), 0, buffer, ' ');

			if (strcmp(buffer, "quit") == 0)
			{
				continue;
			}

			if (buffer[0] != '\0')
			{
				if (Commands.contains(buffer))
				{
					writer->w_string(line.c_str());
				}
			}
		}
	}

	FS.w_close(writer);
	m_cmd_history.clear();
}

void CConsole::ReadLastCmds()
{
	string_path path;
	FS.update_path(path, _app_data_root_, m_config_name);

	if (IReader* reader = FS.r_open(path))
	{
		string1024 line;

		while (!reader->eof())
		{
			reader->r_string(line, sizeof(line));
			m_cmd_history.emplace_back(line);
		}
		FS.r_close(reader);
	}
}

void CConsole::AddLogEntry(const char* line) 
{
	xrCriticalSectionGuard guard(&m_log_history_guard);

	m_log_history.Get(m_log_history.GetHead())._set(line);
	m_log_history.MoveHead(1);
}

void CConsole::ClearLog() 
{
	xrCriticalSectionGuard guard(&m_log_history_guard);

	for (u32 i = 0; i < m_log_history.GetSize(); ++i)
	{
		m_log_history.Get(i)._set(nullptr);
	}
}

void CConsole::AddCommand(IConsole_Command* cc)
{
	Commands.insert({ cc->Name(), cc });
}

void CConsole::RemoveCommand(IConsole_Command* cc)
{
	std::erase_if(Commands, [cc](const auto& entry)
	{
		return entry.second == cc;
	});
}

void CConsole::OnFrame()
{
	PROF_EVENT(__FUNCTION__);

	m_editor->on_frame();
	
	if (Device.dwFrame % 10 == 0) 
	{
		update_tips();
	}
}

void CConsole::OutFont( const char* text, float& pos_y )
{
	CGameFont* pFont = g_FontManager->GetFont(m_fontConsoleName);
	float str_length = pFont->SizeOf_( text );
	float scr_width  = 0.99f * static_cast<float>(Device.TargetWidth);
	if (str_length > scr_width) //1024.0f
	{
		int sz = 0;
		int ln = 0;
		char* one_line = (char*)_alloca((CONSOLE_BUF_SIZE + 1) * sizeof(char));

		while (text[sz] && ln + sz < CONSOLE_BUF_SIZE - 5)// перенос строк
		{
			one_line[ln + sz] = text[sz];
			one_line[ln + sz + 1] = 0;

			if (float t = pFont->SizeOf_(one_line + ln); t > scr_width)
			{
				OutFont(text + sz + 1, pos_y);
				pos_y -= m_line_height;
				pFont->OutI(-0.99f, pos_y, "%s", one_line + ln);
				ln = sz + 1;
			}

			++sz;
		}
	}
	else
	{
		pFont->OutI(-0.99f, pos_y, "%s", text );
	}
}

void CConsole::OnRender()
{
	PROF_EVENT(__FUNCTION__);

	if ( !bVisible )
	{
		return;
	}

	if (!m_hShader_back)
	{
		m_hShader_back = new FactoryPtr<IUIShader>();
		(*m_hShader_back)->create( "hud\\default", "ui\\ui_console" ); // "ui\\ui_empty"
	}

	CGameFont* pFont = g_FontManager->GetFont(m_fontConsoleName);
	CGameFont* pFont2 = g_FontManager->GetFont(m_fontConsole2Name);

	pFont->SetAligment(CGameFont::alLeft);
	pFont2->SetAligment(CGameFont::alLeft);

	m_prompt_width = pFont->WidthOf(ioc_prompt);
	m_cursor_width = pFont->WidthOf(ch_cursor);

	m_line_height = 2.0f * pFont->CurrentHeight_() / static_cast<float>(Device.TargetHeight);

	bool bGame = g_dedicated_server ? false : (g_pGameLevel && g_pGameLevel->bReady) || (g_pGamePersistent && g_pGamePersistent->m_pMainMenu && g_pGamePersistent->m_pMainMenu->IsActive());
	DrawBackgrounds( bGame );
	
	float fMaxY = bGame ? 0.0f : 1.0f;
	float maxStrWidth = Device.TargetWidth * 0.9f; // max cmd str width

	float outY = fMaxY - m_line_height * 1.1f;
	float relativeX = 2.0f / static_cast<float>(Device.TargetWidth);

	const char* strBeforeCursor = ec().str_before_cursor();
	const char* strBeforeSelected = ec().str_before_mark();
	const char* strSelected = ec().str_mark();
	const char* strAfterSelected = ec().str_after_mark();

	float strWidth = m_prompt_width + pFont->WidthOf(strBeforeCursor);

	float outX = 0.0f;
	if (strWidth > maxStrWidth)
	{
		outX -= strWidth - maxStrWidth;
	}

	pFont->SetColor(prompt_font_color);
	pFont->OutI(-1.0f + outX * relativeX, outY, "%s", ioc_prompt);
	outX += m_prompt_width;

	if (!m_disable_tips && !m_tips.empty())
	{
		pFont->SetColor( tips_font_color );

		float shift_x = 0.0f;
		switch (m_tips_mode) 
		{
		case 0: shift_x = relativeX;			break;
		case 1: shift_x = relativeX * outX;		break;
		case 2: shift_x = relativeX * (m_prompt_width + pFont->SizeOf_(m_cur_cmd.c_str()) + m_cursor_width);	break;
		case 3: shift_x = relativeX * strWidth;	break;
		}

		vecTipsEx::iterator itb = m_tips.begin() + m_start_tip;
		vecTipsEx::iterator ite = m_tips.end();
		for (u32 i = 0; itb != ite; ++itb, ++i)
		{ // tips
			pFont->OutI(-1.0f + shift_x, fMaxY + i * m_line_height, "%s", itb->text.c_str());
			if (i >= VIEW_TIPS_COUNT - 1)
			{
				break;
			}
		}	
	}

	// ===== ==============================================
	pFont->SetColor ( cmd_font_color );
	pFont2->SetColor( cmd_font_color );

	pFont->OutI(-1.0f + outX * relativeX, outY, "%s", strBeforeSelected);
	outX += pFont->SizeOf_(strBeforeSelected);
	pFont2->OutI(-1.0f + outX * relativeX, outY, "%s", strSelected);
	outX += pFont2->SizeOf_(strSelected);
	pFont->OutI(-1.0f + outX * relativeX, outY, "%s", strAfterSelected);

	//pFont2->OutI( -1.0f + ioc_d * scr_x, ypos, "%s", editor=all );
	
	if( ec().cursor_view() )
	{
		pFont->SetColor( cursor_font_color );		
		pFont->OutI(-1.0f + strWidth * relativeX, outY, "%s", ch_cursor);
	}
	
	// ---------------------
	m_log_history_guard.Enter();
	u32 log_line = m_log_history.GetTail();
	outY -= m_line_height;
	for (u32 i = scroll_delta; i < log_line; ++i)
	{
		const shared_str& logLine = m_log_history.GetLooped(m_log_history.GetTail() - i);

		if (!logLine.size()) {
			continue;
		}

		outY -= m_line_height;
		if (outY < -1.0f)
		{
			break;
		}

		const char* ls = logLine.c_str();

		Console_mark cm = (Console_mark)ls[0];
		pFont->SetColor( get_mark_color( cm ) );
		OutFont( ls, outY);
	}
	m_log_history_guard.Leave();

	string16 q;
	_itoa( log_line, q, 10 );
	u32 qn = xr_strlen( q );
	pFont->SetColor( total_font_color );
	pFont->OutI( 0.95f - 0.03f * qn, fMaxY - 2.0f * m_line_height, "[%d]", log_line );
	pFont->OnRender();
	pFont2->OnRender();
}

void CConsole::DrawBackgrounds(bool bGame) 
{
	float ky = bGame? 0.5f : 1.0f;

	Frect r;
	r.set( 0.0f, 0.0f, static_cast<float>(Device.TargetWidth), ky * static_cast<float>(Device.TargetHeight) );

	UIRender->SetShader( **m_hShader_back );
	// 6 = back, 12 = tips, (VIEW_TIPS_COUNT+1)*6 = highlight_words, 12 = scroll
	UIRender->StartPrimitive(6 + 12 + (VIEW_TIPS_COUNT + 1) * 6 + 12, IUIRender::ptTriList, IUIRender::pttTL);

	DrawRect( r, back_color );

	if (m_tips.empty() || m_disable_tips) 
	{
		UIRender->FlushPrimitive();
		return;
	}

	CGameFont* pFont = g_FontManager->GetFont(m_fontConsoleName);

	int maxStrWidth = pFont->WidthOf("xxxxx");
	for (const TipString& itb : m_tips)
	{
		int strWidth = pFont->WidthOf(itb.text.c_str());
		maxStrWidth = std::max(strWidth, maxStrWidth);
	}

	float cmdWidth = 0;
	if (m_cur_cmd.size() > 0) 
	{
		cmdWidth = m_cursor_width + pFont->SizeOf_(m_cur_cmd.c_str());
	}

	float fontHeight = pFont->CurrentHeight_();
	float tipsHeight = std::min(m_tips.size(), (size_t)VIEW_TIPS_COUNT) * fontHeight + 5;

	Frect pr, sr; //background rect, selection rect
	pr.x1 = m_prompt_width - m_cursor_width + cmdWidth;
	pr.x2 = pr.x1 + maxStrWidth + 2 * m_cursor_width;

	pr.y1 = UI_BASE_HEIGHT * 0.5f;
	pr.y1 *= static_cast<float>(Device.TargetHeight) /UI_BASE_HEIGHT;

	pr.y2 = pr.y1 + tipsHeight;

	float select_y = 0.0f;
	float select_h = 0.0f;
	
	if (m_select_tip >= 0 && std::cmp_less(m_select_tip, m_tips.size()))
	{
		int sel_pos = m_select_tip - m_start_tip;

		select_y = sel_pos * fontHeight;
		select_h = fontHeight; //1 string
	}
	
	sr.x1 = pr.x1;
	sr.y1 = pr.y1 + select_y;

	sr.x2 = pr.x2;
	sr.y2 = sr.y1 + select_h;

	DrawRect( pr, tips_back_color );
	DrawRect( sr, tips_select_color );

	// --------------------------- highlight words --------------------

	if (std::cmp_less(m_select_tip, m_tips.size()))
	{
		Frect rect {};

		xr_string tmp;
		u32 end_index = std::min<u32>(m_start_tip + VIEW_TIPS_COUNT, m_tips.size());

		for (u32 idx = m_start_tip; idx < end_index; ++idx)
		{
			TipString const& ts = m_tips[idx];
			if (ts.begin < 0 || ts.end < 0 || ts.begin > ts.end)
			{
				continue;
			}

			if (u32 str_size = ts.text.size(); std::cmp_greater_equal(ts.begin, str_size) || std::cmp_greater(ts.end, str_size))
			{
				continue;
			}
			
			u32 i = idx - m_start_tip;
			rect.null();
			
			tmp.assign(ts.text.c_str(), ts.begin);
			rect.x1 = pr.x1 + m_cursor_width + pFont->SizeOf_(tmp.c_str());
			rect.y1 = pr.y1 + i * fontHeight;

			tmp.assign(ts.text.c_str(), ts.end);
			rect.x2 = pr.x1 + m_cursor_width + pFont->SizeOf_(tmp.c_str());
			rect.y2 = rect.y1 + fontHeight;

			DrawRect(rect, tips_word_color);
		}
	}

	// --------------------------- scroll bar --------------------

	size_t tips_sz = m_tips.size();
	if (tips_sz > VIEW_TIPS_COUNT)
	{
		Frect rb, rs;
		
		rb.x1 = pr.x2;
		rb.y1 = pr.y1;
		rb.x2 = rb.x1 + 2 * m_cursor_width;
		rb.y2 = pr.y2;
		DrawRect( rb, tips_scroll_back_color );

		VERIFY( rb.y2 - rb.y1 >= 1.0f );
		
		float back_height = rb.y2 - rb.y1;
		float u_height = back_height * (float)VIEW_TIPS_COUNT / static_cast<float>(tips_sz);
		
		u_height = std::max(u_height, 0.5f * fontHeight);
		
		float u_pos = back_height * static_cast<float>(m_start_tip) / static_cast<float>(tips_sz);

		rs = rb;
		rs.y1 = pr.y1 + u_pos;
		rs.y2 = rs.y1 + u_height;
		DrawRect( rs, tips_scroll_pos_color );
	}

	UIRender->FlushPrimitive();
}

void CConsole::DrawRect(Frect const& r, u32 color)
{
	UIRender->PushPoint( r.x1, r.y1, 0.0f, color, 0.0f, 0.0f );
	UIRender->PushPoint( r.x2, r.y1, 0.0f, color, 1.0f, 0.0f );
	UIRender->PushPoint( r.x2, r.y2, 0.0f, color, 1.0f, 1.0f );

	UIRender->PushPoint( r.x1, r.y1, 0.0f, color, 0.0f, 0.0f );
	UIRender->PushPoint( r.x2, r.y2, 0.0f, color, 1.0f, 1.0f );
	UIRender->PushPoint( r.x1, r.y2, 0.0f, color, 0.0f, 1.0f );
}

void CConsole::ExecuteCommand(const char* cmd_str, bool record_cmd, bool Silent)
{
	u32  str_size = xr_strlen( cmd_str );	
	char* edt = new char[str_size + 1];
	char* first = new char[str_size + 1];
	char* last = new char[str_size + 1];

	xr_strcpy( edt, str_size+1, cmd_str );
	edt[str_size] = 0;

	scroll_delta	= 0;

	if (!Silent)
	{
		reset_cmd_history_idx();
		reset_selected_tip();
	}

	text_editor::remove_spaces(edt);
	if ( edt[0] == 0 )
	{
		return;
	}
	if ( record_cmd )
	{
		char c[2];
		c[0] = mark2;
		c[1] = 0;

		if ( m_last_cmd.c_str() == nullptr || xr_strcmp( m_last_cmd, edt ) != 0 )
		{
			Msg("%s %s", c, edt);
			add_cmd_history( edt );
			m_last_cmd = edt;
		}
	}
	text_editor::split_cmd( first, last, edt );

	// search
	if (vecCMD_IT it = Commands.find(first); it != Commands.end())
	{
		if (IConsole_Command* cc = it->second; cc && cc->bEnabled )
		{
			if ( cc->bLowerCaseArgs )
			{
				_strlwr( last );
			}
			
			if ( last[0] == 0 )
			{
				if ( cc->bEmptyArgsHandled )
				{
					cc->Execute( last );
				}
				else
				{
					IConsole_Command::TStatus stat;
					cc->Status( stat );
					Msg( "- %s %s", cc->Name(), stat );
				}
			}
			else
			{
				cc->Execute( last );
				if ( record_cmd )
				{
					cc->add_to_LRU(last);
				}
			}
		}
		else
		{
			Log("! Command disabled.");
		}
	}
	else
	{
		Msg( "! Unknown command: %s", first );
	}

	xr_delete(edt);
	xr_delete(first);
	xr_delete(last);

	if (record_cmd)
	{
		ec().clear_states();
	}
}

void CConsole::Show()
{
	if (bVisible)
	{
		return;
	}

	bVisible = true;
	scroll_delta = 0;

	update_tips();
	m_editor->IR_Capture();

	Device.seqRender.Add(this, 1);
	Device.seqFrame.Add(this);
}

void CConsole::Hide()
{
	if (!bVisible || (g_pGamePersistent && g_dedicated_server))
	{
		return;
	}

	bVisible = false;

	update_tips();

	Device.seqFrame.Remove(this);
	Device.seqRender.Remove(this);

	m_editor->IR_Release();
}

void CConsole::Clear()
{
	Console->ClearLog();
	xrLogger::FlushLog();
}

void CConsole::SelectCommand()
{
	if ( m_cmd_history.empty() )
	{
		return;
	}
	VERIFY( 0 <= m_cmd_history_idx && m_cmd_history_idx < (int)m_cmd_history.size() );

	vecHistory::reverse_iterator it_rb = m_cmd_history.rbegin() + m_cmd_history_idx;
	ec().set_edit(it_rb->c_str());
	reset_selected_tip();
}

void CConsole::Execute( const char* cmd )
{
	ExecuteCommand( cmd, false );
}

void CConsole::ExecuteScript( const char* str )
{
	xr_string Buffer = "cfg_load ";
	Buffer += str;
	Execute(Buffer.c_str());
}

IConsole_Command* CConsole::find_next_cmd( const char* in_str, shared_str& out_str )
{
	const char* radmin_cmd_name = "ra ";
	bool b_ra  = in_str == xr_strstr(in_str, radmin_cmd_name);
	u32 offset = b_ra? xr_strlen( radmin_cmd_name ) : 0;

	string256 t2;
	xr_strconcat( t2, in_str + offset, " " );

	if (vecCMD_IT it = Commands.lower_bound(t2); it != Commands.end())
	{
		IConsole_Command* cc = it->second;
		const char* name_cmd      = cc->Name();
		u32    name_cmd_size = xr_strlen( name_cmd );
		char*   new_str       = (char*)_alloca( (offset + name_cmd_size + 2) * sizeof(char) );

		xr_strcpy( new_str, offset + name_cmd_size + 2, b_ra? radmin_cmd_name : "" );
		xr_strcat( new_str, offset + name_cmd_size + 2, name_cmd );

		out_str._set( new_str );
		return cc;
	}
	return nullptr;
}

bool CConsole::add_next_cmds(const char* in_str, vecTipsEx& out_v)
{
	size_t cur_count = out_v.size();

	if (cur_count >= MAX_TIPS_COUNT)
	{
		return false;
	}

	string256 t2;
	xr_strconcat(t2, in_str, " ");

	shared_str temp;
	IConsole_Command* cc = find_next_cmd(t2, temp);

	if (!cc || !temp.size())
	{
		return false;
	}

	bool res = false;
	for (u32 i = cur_count; i < MAX_TIPS_COUNT * 2; ++i)
	{
		temp = cc->Name();

		if (!std::ranges::contains(out_v, temp, &TipString::text))
		{
			TipString ts(temp);
			out_v.push_back(ts);
			res = true;
		}

		if (out_v.size() >= MAX_TIPS_COUNT)
		{
			break;
		}

		string256 t3;
		xr_strconcat(t3, out_v.back().text.c_str(), " ");
		cc = find_next_cmd(t3, temp);

		if (!cc)
		{
			break;
		}
	}
	return res;
}

bool CConsole::add_internal_cmds( const char* in_str, vecTipsEx& out_v )
{
	u32 cur_count = (u32)out_v.size();
	u32 in_sz = xr_strlen(in_str);
	
	if (cur_count >= MAX_TIPS_COUNT)
	{
		return false;
	}

	bool res = false;
	xr_string name2; // word in begin

	for (const auto name : Commands | std::views::keys)
	{
		u32 len = xr_strlen(name);

		if (len >= in_sz)
		{
			name2.assign(name, in_sz);
			if (!xr_stricmp(name2.c_str(), in_str))
			{
				shared_str temp = name;

				if (!std::ranges::contains(out_v, temp, &TipString::text))
				{
					out_v.emplace_back(temp, 0, in_sz);
					res = true;
				}
			}
		}

		if (out_v.size() >= MAX_TIPS_COUNT)
		{
			return res;
		}
	}

	// word in internal
	for (const auto name : Commands | std::views::keys)
	{
		if (const char* fd_str = xr_strstr(name, in_str))
		{
			shared_str temp;
			temp._set(name);

			if (!std::ranges::contains(out_v, temp, &TipString::text))
			{
				u32 name_sz = xr_strlen(name);
				int fd_sz = name_sz - xr_strlen(fd_str);
				out_v.emplace_back(temp, fd_sz, fd_sz + in_sz);
				res = true;
			}
		}
		if (out_v.size() >= MAX_TIPS_COUNT)
		{
			return res;
		}
	}

	return res;
}

void CConsole::update_tips() {
	m_temp_tips.clear();
	m_tips.clear();

	m_cur_cmd  = nullptr;
	if (!bVisible) {
		return;
	}

	const char* cur = ec().str_edit();
	u32    cur_length = xr_strlen( cur );

	if (cur_length == 0) {
		m_prev_length_str = 0;
		return;
	}
	
	if (m_prev_length_str != cur_length) {
		reset_selected_tip();
	}
	m_prev_length_str = cur_length;

	char* first = (char*)_alloca( (cur_length + 1) * sizeof(char) );
	char* last  = (char*)_alloca( (cur_length + 1) * sizeof(char) );
	text_editor::split_cmd( first, last, cur );
	
	u32 first_lenght = xr_strlen(first);
	
	if (first_lenght > 2 && first_lenght + 1 <= cur_length) { // param
		if (cur[first_lenght] == ' ') {
			if (m_tips_mode != 2) {
				reset_selected_tip();
			}

			if (vecCMD_IT it = Commands.find(first); it != Commands.end()) {
				IConsole_Command* cc = it->second;
				
				u32 mode = 0;
				if (first_lenght + 2 <= cur_length && cur[first_lenght] == ' ' && cur[first_lenght + 1] == ' ') {
					mode = 1;
					last += 1; // fake: next char
				}

				cc->fill_tips( m_temp_tips, mode );
				m_tips_mode = 2;
				m_cur_cmd._set( first );
				select_for_filter( last, m_temp_tips, m_tips );

				if (m_tips.empty())
				{
					m_tips.emplace_back("(empty)");
				}
				
				if (std::cmp_less_equal(m_tips.size(), m_select_tip)) 
				{
					reset_selected_tip();
				}
				return;
			}
		}
	}

	// cmd name
	{
		add_internal_cmds( cur, m_tips );
		m_tips_mode = 1;
	}

	if (m_tips.empty()) {
		m_tips_mode = 0;
		reset_selected_tip();
	}

	if (std::cmp_less_equal(m_tips.size(), m_select_tip)) {
		reset_selected_tip();
	}

}

void CConsole::select_for_filter(const char* filter_str, const vecTips& in_v, vecTipsEx& out_v)
{
	out_v.clear();

	if (u32 in_count = (u32)in_v.size(); in_count == 0 || !filter_str)
	{
		return;
	}

	for (shared_str const& str : in_v)
	{
		if (xr_strlen(filter_str) == 0)
		{
			out_v.emplace_back(str);
		}
		else
		{
			if (const char* fd_str = xr_strstr(str.c_str(), filter_str))
			{
				int fd_sz = str.size() - xr_strlen(fd_str);
				TipString ts(str, fd_sz, fd_sz + xr_strlen(filter_str));
				out_v.push_back(ts);
			}
		}
	}
}
