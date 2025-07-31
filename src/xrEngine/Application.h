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
	virtual void			Level_Scan();
	virtual int				Level_ID(LPCSTR name, LPCSTR ver, bool bSet);
	virtual void			Level_Set(u32 ID);
	virtual void			LoadAllArchives();
	virtual CInifile* GetArchiveHeader(LPCSTR name, LPCSTR ver);

	// Loading
	virtual void					LoadBegin();
	virtual void					LoadEnd();
	virtual void					LoadTitleInt(LPCSTR str1, LPCSTR str2, LPCSTR str3);
	virtual void					LoadStage();
	virtual void					LoadSwitch();
	virtual void					LoadDraw();
	virtual void					LoadForceFinish();
	virtual void					SetLoadStageTitle(pcstr _ls_title);

	virtual	void			OnEvent(EVENT E, u64 P1, u64 P2);

	// Other
	CApplication();
	virtual					~CApplication();

	virtual void	_BCL	OnFrame();
	virtual void			load_draw_internal();
	void SetLoadingScreen(ILoadingScreen* newScreen);
	virtual void DestroyLoadingScreen();
};

extern ENGINE_API CApplication* pApp;