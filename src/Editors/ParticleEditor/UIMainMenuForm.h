#pragma once
class UIMainMenuForm :public IEditorWnd
{
public:
	UIMainMenuForm();
	virtual ~UIMainMenuForm();
	virtual void Draw();

private:
	shared_str GetCommandShortcat(int CommandID) const;
	void DrawMenuItem(const char* label, int command, int param = 0, int flag = 0);
	void DrawMenuItemI(const char* label, const char* icon, int command, int param = 0, int flag = 0);
};
