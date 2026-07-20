#pragma once

#include "../Include/xrRender/FactoryPtr.h"
#include "../Include/xrRender/UIShader.h"
#include <charconv>

class ENGINE_API CGameFont;
class ENGINE_API IConsole_Command;

#ifdef DEBUG_DRAW
	struct ImGuiSettingsHandler;
	struct ImGuiContext;
	struct ImGuiTextBuffer;
#endif

namespace text_editor
{
	class line_editor;
	class line_edit_control;
}

struct TipString
{
	shared_str text;
	int begin;
	int end;

	TipString()
	{
		text._set("");
		begin = 0;
		end = 0;
	}
	
	TipString(shared_str const& tips_text, int start_pos, int finish_pos)
	{
		text._set(tips_text);
		begin = start_pos;
		end = finish_pos;
	}
	
	TipString(const char* tips_text, int start_pos, int finish_pos)
	{
		text._set(tips_text);
		begin = start_pos;
		end = finish_pos;
	}
	
	TipString(shared_str const& tips_text)
	{
		text._set(tips_text);
		begin = 0;
		end = 0;
	}
	
	ICF bool operator==(shared_str const& tips_text)
	{
		return text == tips_text;
	}
};

class ENGINE_API CConsole :
	public pureRender,
	public pureFrame
{
public:
	struct str_pred
	{
		IC bool operator()(const char* x, const char* y) const
		{
			return xr_strcmp(x, y) < 0;
		}
	};

	using mapCMD = xr_map<const char*,IConsole_Command*,str_pred>;
	using vecCMD_IT = mapCMD::iterator;
	using vecCMD_CIT = mapCMD::const_iterator;
	using Callback = xr_delegate<void()>;
	using vecHistory = xr_vector<shared_str>;
	using vecTips = xr_vector<shared_str>;
	using vecTipsEx = xr_vector<TipString>;

protected:
	static constexpr int CONSOLE_BUF_SIZE = 1024;
	static constexpr int VIEW_TIPS_COUNT = 14;
	static constexpr int MAX_TIPS_COUNT = 220;
	static constexpr u32  RING_BUFFER_SIZE = 262144;
	
	RingBuffer<shared_str, RING_BUFFER_SIZE> m_log_history;
	xrCriticalSection m_log_history_guard;
	FactoryPtr<IUIShader>* m_hShader_back;
	
	int scroll_delta;
	bool m_disable_tips;

private:
	vecHistory m_cmd_history;
	u32 m_cmd_history_max;
	int m_cmd_history_idx;
	shared_str m_last_cmd;

	vecTips m_temp_tips;
	vecTipsEx m_tips;
	u32 m_tips_mode;
	shared_str m_cur_cmd;
	int m_select_tip;
	int m_start_tip;
	u32 m_prev_length_str;

	int m_prompt_width;
	int m_cursor_width;
	float m_line_height;
	const char* m_config_name = "last_cmds.ltx";

#ifdef DEBUG_DRAW
	void ImGuiDrawUIConsole();
	void ImGuiDrawUIConsoleVars();
	ImGuiSettingsHandler* DebugConsoleVarsSettingsHandler;
#endif

public:
	string64 ConfigFile;
	bool bVisible;
	mapCMD Commands;
	
	CConsole();
	virtual ~CConsole();
	virtual void Initialize();
	virtual void Destroy();
	
	void ReadLastCmds();
	void FlushLastCmds();

	void AddLogEntry(const char* line);
	void ClearLog();

	virtual void OnRender();
	virtual void _BCL OnFrame();

	void AddCommand(IConsole_Command* cc);
	void RemoveCommand(IConsole_Command* cc);

	void Show();
	void Hide();
	void Clear();

	void Execute(const char* cmd);
	void ExecuteScript(const char* str);
	void ExecuteCommand(const char* cmd, bool record_cmd = true, bool Silent = true);
	void SelectCommand();

	bool GetBool(const char* cmd) const;
	float GetFloat(const char* cmd, float& min, float& max) const;
	int GetInteger(const char* cmd, int& min, int& max) const;
	const char* GetString(const char* cmd) const;
	const char* GetToken(const char* cmd) const;
	xr_token* GetXRToken(const char* cmd) const;
	Fvector GetFVector(const char* cmd) const;
	Fvector* GetFVectorPtr(const char* cmd) const;
	IConsole_Command* GetCommand(const char* cmd) const;

#ifdef DEBUG_DRAW
	static void* ImGuiReadOpenUIConsoleVars(ImGuiContext* Ctx, ImGuiSettingsHandler* Handler, const char* Name);
	static void ImGuiReadLineUIConsoleVars(ImGuiContext* Ctx, ImGuiSettingsHandler* Handler, void* Entry, const char* Line);
	static void ImGuiWriteAllUIConsoleVars(ImGuiContext* Ctx, ImGuiSettingsHandler* Handler, ImGuiTextBuffer* OutBuf);
	void RegisterImGuiConsoleSettingsHandler();
#endif

protected:
	text_editor::line_editor* m_editor;
	text_editor::line_edit_control& ec();

	enum Console_mark // (int)=char
	{
		no_mark = ' ',
		mark0 = '~',
		mark1 = '!', // error
		mark2 = '@', // console cmd
		mark3 = '#',
		mark4 = '$',
		mark5 = '%',
		mark6 = '^',
		mark7 = '&',
		mark8 = '*',
		mark9 = '-', // green = ok
		mark10 = '+',
		mark11 = '=',
		mark12 = '/'
	};

	bool is_mark(Console_mark type);
	u32 get_mark_color(Console_mark type);

	void DrawBackgrounds(bool bGame);
	void DrawRect(Frect const& r, u32 color);
	void OutFont(const char* text, float& pos_y);
	void Register_callbacks();

	void Prev_log();
	void Next_log();
	void Begin_log();
	void End_log();

	void Find_cmd();
	void Find_cmd_back();
	void Prev_cmd();
	void Next_cmd();
	void Prev_tip();
	void Next_tip();

	void Begin_tips();
	void End_tips();
	void PageUp_tips();
	void PageDown_tips();

	void Execute_cmd();
	void Show_cmd();
	void Hide_cmd();
	void Hide_cmd_esc();
	
	void add_cmd_history(shared_str const& str);
	void next_cmd_history_idx();
	void prev_cmd_history_idx();
	void reset_cmd_history_idx();

	void next_selected_tip();
	void check_next_selected_tip();
	void prev_selected_tip();
	void check_prev_selected_tip();
	void reset_selected_tip();

	IConsole_Command* find_next_cmd(const char* in_str, shared_str& out_str);
	bool add_next_cmds(const char* in_str, vecTipsEx& out_v);
	bool add_internal_cmds(const char* in_str, vecTipsEx& out_v);

	void update_tips();
	void select_for_filter(const char* filter_str, const vecTips& in_v, vecTipsEx& out_v);

}; // class CConsole

ENGINE_API extern CConsole* Console;