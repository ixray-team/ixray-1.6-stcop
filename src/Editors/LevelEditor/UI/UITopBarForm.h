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
	ref_texture m_tCForm;
	ref_texture m_tAIMap;
	ref_texture m_tGGraph;
	ref_texture m_tPlayInEditor;
	ref_texture m_tTerminated;
	ref_texture m_tPlayPC;
	ref_texture m_tPlayCleanGame;
	ref_texture	m_PreferencesIcon;

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