#pragma once
#include "../xrECore/Editor/UIToolbar.h"

class UIMainForm :public IEditorWnd
{
public:
	UIMainForm();
	virtual ~UIMainForm();
	virtual void Draw();
	bool Frame();
	IC UILeftBarForm* GetLeftBarForm() {return m_LeftBar;}
	IC UITopBarForm* GetTopBarForm() { return m_TopBar; }
	IC UIKeyForm* GetKeyForm() { return m_KeyForm; }

	IC UIRenderForm* GetRenderForm() { return m_Render; }
private:
	void DrawRenderToolBar(ImVec2 Pos, ImVec2 Size);

private:
	UITopBarForm *m_TopBar;
	UIRenderForm* m_Render;
	UIMainMenuForm* m_MainMenu;
	UILeftBarForm* m_LeftBar;
	UIKeyForm* m_KeyForm;

    // Action
    ref_texture m_tSelect;
    ref_texture m_tMove;
    ref_texture m_tRotate;
    ref_texture m_tScale;

    // Axis
    ref_texture m_tX;
    ref_texture m_tY;
    ref_texture m_tZ;
    ref_texture m_tZX;
};
extern UIMainForm* MainForm;
