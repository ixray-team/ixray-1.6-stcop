#pragma once

class XRayEditor :
	public CEngineApp
{
public:
	XRayEditor();
	virtual	~XRayEditor();
	virtual void					Level_Scan();
	virtual int						Level_ID(const char* name, const char* ver, bool bSet);
	virtual void					Level_Set(u32 ID);
	virtual void					LoadAllArchives();
	virtual CInifile* GetArchiveHeader(const char* name, const char* ver);

	// Loading
	virtual void					LoadBegin();
	virtual void					LoadEnd();
	virtual void					LoadTitleInt(const char* str1, const char* str2, const char* str3);
	virtual void					LoadStage();
	virtual void					LoadSwitch();
	virtual void					LoadDraw();

	virtual	void			OnEvent(EVENT E, u64 P1, u64 P2);


	virtual void	_BCL	OnFrame();
	virtual	void			load_draw_internal();
	virtual	void			DestroyLoadingScreen();
};