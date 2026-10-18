#pragma once
#include "../xrCore/EventAPI.h"

class ILoadingScreen;

class ENGINE_API CEngineApp :
	public pureFrame,
	public IEventReceiver
{
	// levels
	struct sLevelInfo
	{
		char* folder;
		char* name;
	};

private:
	ILoadingScreen* GLoadingScreen;

	xr_atomic_s32 max_load_stage;

	xr_atomic_s32 load_stage;

	u32 ll_dwReference;

	void Level_Append(const char* lname);

public:
	// Levels
	xr_vector<sLevelInfo> Levels;
	u32 Level_Current;
	virtual void Level_Scan();
	virtual int Level_ID(const char* name, const char* ver, bool bSet);
	virtual void Level_Set(u32 ID);
	virtual void LoadAllArchives();
	virtual CInifile* GetArchiveHeader(const char* name, const char* ver);

	// Loading
	virtual void LoadBegin();
	virtual void LoadEnd();
	virtual void LoadTitleInt(const char* str1, const char* str2, const char* str3);
	virtual void LoadStage();
	virtual void LoadSwitch();
	virtual void LoadDraw();
	void LoadPulse();
	virtual void LoadForceFinish();
	virtual void SetLoadStageTitle(const char* _ls_title);

	virtual void OnEvent(EVENT E, u64 P1, u64 P2);

	// Other
	CEngineApp();
	virtual ~CEngineApp();

	virtual void OnFrame();
	virtual void load_draw_internal();
	void SetLoadingScreen(ILoadingScreen* newScreen);
	virtual void DestroyLoadingScreen();

private:
	static constexpr u32 LoadPulsePeriod = 50;
	u32 LastLoadDraw = 0;
	xrCriticalSection PendingStageTitleLock;
	xr_string PendingStageTitle;
	xr_string PendingLevelLogo;
	xr_string PendingTip[3];
	bool IsTipPending = false;
};

extern ENGINE_API CEngineApp* pApp;
