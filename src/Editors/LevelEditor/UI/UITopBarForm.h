#pragma once
class UITopBarForm :
	public IEditorWnd
{
public:
	UITopBarForm();
	virtual ~UITopBarForm();
	virtual void Draw();
	bool VerifySpaceRestrictors()const { return m_VerifySpaceRestrictors; }
	bool UseCameraPosForActor = false;

private:
	u32 m_timeUndo;
	u32 m_timeRedo;

	ref_texture m_tReloadConfigs;
	ref_texture m_tAIMap;
	ref_texture m_tPlayInEditor;
	ref_texture m_tTerminated;
	ref_texture m_tPlayPC;
	ref_texture m_tPlayCleanGame;

	bool m_VerifySpaceRestrictors;
	bool m_Simulate;

	void ClickUndo();
	void ClickRedo();

	void ClickNew();
	void ClickOpen();
	void ClickSave();

	void ClickReloadConfigs();
	void ClickOpenGameData();

	void ClickCForm();
	void ClickAIMap();
	void ClickGGraph();
	void ClickPlayInEditor();
	void ClickBuildAndMake();
	void ClickTerminated();
	void ClickPlayPC();
	void ClickPlayCleanGame();

	void ClickPreferences();

private:
	xr_string_map<xr_string, ImVec2> TableSizes;
	void ApplyBackground(const xr_string& TableColumName);
	void CalcTableEndPos(const xr_string& TableColumName);
};