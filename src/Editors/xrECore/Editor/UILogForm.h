#pragma once

class ECORE_API UILogForm 
{
	friend class CLevelPreferences;

public:
	static void AddMessage( const xr_string& msg);
	static void Show();
	static void SetActive();
	static void Hide();
	static void Update();
	// The window is not drawn while it returns true: workspaces with their own messages
	static bool (*HideFunctor)();
	static void Destroy();

	static void Clear();
	static bool ClearInPIE();

private:
	static xr_vector<xr_string>*List;
	static xr_vector<xr_string>* GetList();
	static xr_vector<bool*> SelectedItems;
	static bool bAutoScroll;
	static bool bClearInPIE;
	static string_path Filter;
	static string_path Exec;

	// Console command suggestions for the exec input
	static constexpr size_t MaxSuggestions = 12;
	static xr_vector<class IConsole_Command*> Suggestions;
	static int SuggestionIndex;
	static ImVec2 SuggestionsMin;
	static ImVec2 SuggestionsMax;
	static bool IsSuggestionsHovered();
	static bool IsExecFocusRequested;
	static void CollectSuggestions();
	static void DrawSuggestions(bool IsInputActive, const ImVec2& InputMin, float InputWidth);
	static int ExecInputCallback(struct ImGuiInputTextCallbackData* Data);
};