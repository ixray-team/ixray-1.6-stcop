#pragma once
class UIMainMenuForm :public IEditorWnd
{
public:
	UIMainMenuForm();
	virtual ~UIMainMenuForm();
	virtual void Draw();
	void DrawLevelName();
	void ExportLevelAsArchive();

private:
	shared_str GetCommandShortcat(int CommandID) const;
	void DrawMenuItem(const char* label, int command, const xr_string& param, int flag = 0);
	void DrawMenuItem(const char* label, int command, int param = 0, int flag = 0);

	ref_texture PlugPy;
	ref_texture PlugLua;
};

