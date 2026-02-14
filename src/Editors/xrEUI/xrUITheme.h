#pragma once

class XREUI_API CUIThemeManager :
	public IEditorWnd
{

	CUIThemeManager();
	~CUIThemeManager() = default;
	
public:
	static CUIThemeManager& Get();
	void InitDefault(int ThemeID = -1);
	void Show(bool value);

protected:
	virtual void Draw();
	bool IsLoaded = false;

public:
	float TransparentDefault = 1.f;
	float TransparentUnfocused = 0.33f;

	int ThemeID = 0;

	// St4lker0k765: customizable log message colors
	ImVec4 log_color_default;
	ImVec4 log_color_error;
	ImVec4 log_color_warning;
	ImVec4 log_color_debug;
};