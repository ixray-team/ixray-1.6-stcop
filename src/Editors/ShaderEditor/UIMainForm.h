#pragma once
class UIMainForm :public IEditorWnd
{
public:
	UIMainForm();
	virtual ~UIMainForm();
	virtual void Draw();
	bool Frame();
	IC UILeftBarForm* GetLeftBarForm() {return m_LeftBar;}
	IC UITopBarForm* GetTopBarForm() { return m_TopBar; }
private:
	void DrawRenderToolBar(ImVec2 Pos, ImVec2 Size);

	UITopBarForm *m_TopBar;
	UIRenderForm* m_Render;
	UIMainMenuForm* m_MainMenu;
	UILeftBarForm* m_LeftBar;
};
extern UIMainForm* MainForm;
