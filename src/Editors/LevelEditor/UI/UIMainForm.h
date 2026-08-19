#pragma once
class UIMaterialEditorForm;
class UIMaterialInstanceEditorForm;
class CSceneObject;

class UIMainForm final :
	public IEditorWnd
{
public:
	UIMainForm();
	virtual ~UIMainForm();

	virtual void Draw() override;
	virtual void ResetEnd() override;

	void LoadWindowsStates();
	bool Frame();
	IC UILeftBarForm* GetLeftBarForm() { return m_LeftBar; }
	IC UITopBarForm* GetTopBarForm() { return m_TopBar; }
	IC UIRenderForm* GetRenderForm() { return m_Render; }
	IC UILPropertiesForm* GetPropertiesForm() { return m_Properties; }
	IC class UIWorldPropertiesFrom* GetWorldPropertiesFrom() { return m_WorldProperties; }
	IC UIMaterialEditorForm* GetMaterialEditorForm() { return m_MaterialEditor; }
	IC UIMaterialInstanceEditorForm* GetMaterialInstanceEditorForm()
	{
		return m_MaterialInstanceEditor;
	}
	void OpenMaterialPicker(
		CSceneObject* Object,
		const char* SurfaceName,
		const char* CurrentMaterial
	);

private:
	void DrawContextMenu();
	void DrawRenderToolBar(ImVec2 Pos, ImVec2 Size);
	void DrawMenuSettings();
	void RenderOldCameraButtons();
	void RenderAxisButtons();
	void UpdateMaterialPicker();

	shared_str GetCommandShortcat(int CommandID) const;
	void DrawMenuItem(const char* label, int command, int param = 0, int flag = 0);
	void DrawMenuItemI(const char* label, const char* icon, int command, int param = 0, int flag = 0);

private:
	UITopBarForm* m_TopBar;
	UIRenderForm* m_Render;
	UIMainMenuForm* m_MainMenu;
	UILeftBarForm* m_LeftBar;
	UILPropertiesForm* m_Properties;
	class UIWorldPropertiesFrom* m_WorldProperties;
	UIMaterialEditorForm* m_MaterialEditor;
	UIMaterialInstanceEditorForm* m_MaterialInstanceEditor;
	CSceneObject* m_MaterialPickerObject = nullptr;
	xr_string m_MaterialPickerSurface;
	bool m_MaterialPickerActive = false;

	xr_string m_tMenu;

	// Action
	xr_string m_tSelect;
	xr_string m_tAdd;
	xr_string m_tMove;
	xr_string m_tRotate;
	xr_string m_tScale;

	// Snap
	xr_string m_tGSnap;
	xr_string m_tOSnap;
	xr_string m_tMoveToSnap;
	xr_string m_tNSnap;
	xr_string m_tVSnap;
	xr_string m_tASnap;
	xr_string m_tMSnap;

	xr_string m_tZoom;
	xr_string m_tZoomSel;

	xr_string m_tGrid;
	xr_string m_tScaleGrid;
	xr_string m_tAngle;

	xr_string m_tCsLocal;
	xr_string m_tNuScale;
	xr_string TransformLocalOrWorld;
	xr_string TransformLocalOrWorld2;

	// Axis
	xr_string m_tX;
	xr_string m_tY;
	xr_string m_tZ;
	xr_string m_tZX;

	// View
	xr_string m_tVFront;
	xr_string m_tVBack;
	xr_string m_tVLeft;
	xr_string m_tVRight;
	xr_string m_tVTop;
	xr_string m_tVBottom;
	xr_string m_tVReset;

	// Camera
	xr_string m_tPlaneMove;
	xr_string m_tArcBall;
	xr_string m_tFreeFly;
};
extern UIMainForm* MainForm;
