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
	ILoadingScreen* GLoadingScreen;

	int		max_load_stage;

	int						load_stage;

	u32						ll_dwReference;

	void					Level_Append(const char* lname);
public:

	// Levels
	xr_vector<sLevelInfo>	Levels;
	u32						Level_Current;
	virtual void			Level_Scan();
	virtual int				Level_ID(const char* name, const char* ver, bool bSet);
	virtual void			Level_Set(u32 ID);
	virtual void			LoadAllArchives();
	virtual CInifile* GetArchiveHeader(const char* name, const char* ver);

	// Loading
	virtual void					LoadBegin();
	virtual void					LoadEnd();
	virtual void					LoadTitleInt(const char* str1, const char* str2, const char* str3);
	virtual void					LoadStage();
	virtual void					LoadSwitch();
	virtual void					LoadDraw();
	virtual void					LoadForceFinish();
	virtual void					SetLoadStageTitle(const char* _ls_title);

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