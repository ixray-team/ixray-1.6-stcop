#pragma once
class UIMainMenuForm :public IEditorWnd
{
public:
	UIMainMenuForm();
	virtual ~UIMainMenuForm();
	virtual void Draw();

private:
	shared_str GetCommandShortcat(int CommandID) const;
	void DrawMenuItemI(const char* label, const char* icon, int command, const xr_string& param, int flag = 0);
	void DrawMenuItemI(const char* label, const char* icon, int command, int param = 0, int flag = 0);
};
