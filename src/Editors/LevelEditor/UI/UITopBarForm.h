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
	void InitIcons();

	xr_hash_map<xr_string, xr_string> Icons;

	u32 m_timeUndo;
	u32 m_timeRedo;

	xr_string m_tReloadConfigs;
	xr_string m_tAIMap;
	xr_string m_tPlayInEditor;
	xr_string m_tTerminated;
	xr_string m_tPlayPC;
	xr_string m_tPlayCleanGame;

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
};
