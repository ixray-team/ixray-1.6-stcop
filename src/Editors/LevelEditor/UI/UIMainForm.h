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
    IC UIRenderForm* GetRenderForm() { return m_Render; }
    IC UILPropertiesFrom* GetPropertiesFrom() { return m_Properties; }
    IC class UIWorldPropertiesFrom* GetWorldPropertiesFrom() { return m_WorldProperties; }

private:
    void DrawContextMenu();
    void DrawRenderToolBar(ImVec2 Pos, ImVec2 Size);
    void RenderOldCameraButtons();
    void RenderAxisButtons();

private:
    UITopBarForm* m_TopBar;
    UIRenderForm* m_Render;
    UIMainMenuForm* m_MainMenu;
    UILeftBarForm* m_LeftBar;
    UILPropertiesFrom* m_Properties;
    class UIWorldPropertiesFrom* m_WorldProperties;

    ref_texture m_tMenu;

    // Action
    ref_texture m_tSelect;
    ref_texture m_tAdd;
    ref_texture m_tMove;
    ref_texture m_tRotate;
    ref_texture m_tScale;

    // Snap
    ref_texture m_tGSnap;
    ref_texture m_tOSnap;
    ref_texture m_tMoveToSnap;
    ref_texture m_tNSnap;
    ref_texture m_tVSnap;
    ref_texture m_tASnap;
    ref_texture m_tMSnap;

    ref_texture m_tZoom;
    ref_texture m_tZoomSel;

    ref_texture m_tGrid;
    ref_texture m_tScaleGrid;
    ref_texture m_tAngle;

    ref_texture m_tCsLocal;
    ref_texture m_tNuScale;

    // Axis
    ref_texture m_tX;
    ref_texture m_tY;
    ref_texture m_tZ;
    ref_texture m_tZX;

    // View
    ref_texture m_tVFront;
    ref_texture m_tVBack;
    ref_texture m_tVLeft;
    ref_texture m_tVRight;
    ref_texture m_tVTop;
    ref_texture m_tVBottom;
    ref_texture m_tVReset;

    // Camera
    ref_texture m_tPlaneMove;
    ref_texture m_tArcBall;
    ref_texture m_tFreeFly;
};
extern UIMainForm* MainForm;
