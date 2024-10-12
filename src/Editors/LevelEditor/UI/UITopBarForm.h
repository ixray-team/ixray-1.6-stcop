#pragma once
class UITopBarForm :public IEditorWnd
{
public:
	UITopBarForm();
	virtual ~UITopBarForm();
	virtual void Draw();
	bool VerifySpaceRestrictors()const {return m_VerifySpaceRestrictors;}
private:

	void ClickUndo(); ref_texture m_tUndo; u32 m_timeUndo;
	void ClickRedo(); ref_texture m_tRedo; u32 m_timeRedo;

	void ClickNew(); ref_texture m_tNew;
	void ClickOpen(); ref_texture m_tOpen;
	void ClickSave(); ref_texture m_tSave;

	void ClickReloadConfigs();  ref_texture m_tReloadConfigs;
	void ClickOpenGameData();  ref_texture m_tOpenGameData;

	void ClickCForm();  ref_texture m_tCForm;
	void ClickAIMap();  ref_texture m_tAIMap;
	void ClickGGraph();  ref_texture m_tGGraph;
	void ClickPlayInEditor();  ref_texture m_tPlayInEditor;
	void ClickBuildAndMake(); ref_texture m_tBuildAndMake;
	void ClickTerminated();  ref_texture m_tTerminated;
	void ClickPlayPC();  ref_texture m_tPlayPC;
	void ClickPlayCleanGame();  ref_texture m_tPlayCleanGame;
	bool m_VerifySpaceRestrictors;
};