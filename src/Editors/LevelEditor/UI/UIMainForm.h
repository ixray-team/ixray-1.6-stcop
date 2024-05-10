#pragma once
class UIMainForm :public XrUI
{
public:
	UIMainForm();
	virtual ~UIMainForm();
	virtual void Draw();
	bool Frame();
	IC UILeftBarForm* GetLeftBarForm() {return m_LeftBar;}
	IC UITopBarForm* GetTopBarForm() { return m_TopBar; }
	IC UIRenderForm* GetRenderForm() { return m_Render; }
	IC UILPropertiesFrom* GetPropertiesFrom() { return m_Properties; }
	IC class UIWorldPropertiesFrom* GetWorldPropertiesFrom() { return m_WorldProperties; }
private:
	UITopBarForm *m_TopBar;
	UIRenderForm* m_Render;
	UIMainMenuForm* m_MainMenu;
	UILeftBarForm* m_LeftBar;
	UILPropertiesFrom* m_Properties;
	class UIWorldPropertiesFrom* m_WorldProperties;
private:
	void DrawContextMenu();
	void DrawRenderToolBar(ImVec2 Size);
private:
	ref_texture m_tMenu;

	ref_texture m_tSelect;
	ref_texture m_tAdd;
	ref_texture m_tMove;
	ref_texture m_tRotate;
	ref_texture m_tScale;

	ref_texture m_tNSnap;
	ref_texture m_tZoomSel;

	ref_texture m_tGrid;
	ref_texture m_tScaleGrid;
	ref_texture m_tAngle;

};
extern UIMainForm* MainForm;
