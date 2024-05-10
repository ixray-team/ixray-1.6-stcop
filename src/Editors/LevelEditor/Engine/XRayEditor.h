#pragma once

class XRayEditor :public CApplication
{
public:
	XRayEditor();
	virtual	~XRayEditor();
	virtual void					Level_Scan();
	virtual int						Level_ID(LPCSTR name, LPCSTR ver, bool bSet);
	virtual void					Level_Set(u32 ID);
	virtual void					LoadAllArchives();
	virtual CInifile* GetArchiveHeader(LPCSTR name, LPCSTR ver);

	// Loading
	virtual void					LoadBegin();
	virtual void					LoadEnd();
	virtual void					LoadTitleInt(LPCSTR str1, LPCSTR str2, LPCSTR str3);
	virtual void					LoadStage();
	virtual void					LoadSwitch();
	virtual void					LoadDraw();

	virtual	void			OnEvent(EVENT E, u64 P1, u64 P2);


	virtual void	_BCL	OnFrame();
	virtual	void			load_draw_internal();
	virtual	void			destroy_loading_shaders();
};