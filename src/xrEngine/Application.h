#pragma once
#include "../xrCore/EventAPI.h"

class ILoadingScreen;

class ENGINE_API CApplication :
	public pureFrame,
	public IEventReceiver
{
	// levels
	struct					sLevelInfo
	{
		char* folder;
		char* name;
	};

private:
	ILoadingScreen* loadingScreen;

	int		max_load_stage;

	int						load_stage;

	u32						ll_dwReference;

	void					Level_Append(LPCSTR lname);
public:

	// Levels
	xr_vector<sLevelInfo>	Levels;
	u32						Level_Current;
	void					Level_Scan();
	int						Level_ID(LPCSTR name, LPCSTR ver, bool bSet);
	void					Level_Set(u32 ID);
	void					LoadAllArchives();
	CInifile* GetArchiveHeader(LPCSTR name, LPCSTR ver);

	// Loading
	void					LoadBegin();
	void					LoadEnd();
	void					LoadTitleInt(LPCSTR str1, LPCSTR str2, LPCSTR str3);
	void					LoadStage();
	void					LoadSwitch();
	void					LoadDraw();
	void LoadForceFinish();

	virtual	void			OnEvent(EVENT E, u64 P1, u64 P2);

	// Other
	CApplication();
	virtual					~CApplication();

	virtual void	_BCL	OnFrame();
	void			load_draw_internal();
	void SetLoadingScreen(ILoadingScreen* newScreen);
	void DestroyLoadingScreen();
};

extern ENGINE_API CApplication* pApp;