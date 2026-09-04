#pragma once
#include "../../Entry/Planars/planar.h"

class EScenePlanarsTool :
	public ESceneCustomOTool
{
	typedef ESceneCustomOTool inherited;
	friend class SceneBuilder;

protected:
	virtual void		CreateControls			();
	virtual void		RemoveControls			();

public:
						EScenePlanarsTool		();
	virtual				~EScenePlanarsTool		();

	virtual void		Clear					(bool bSpecific = false);

	IC const char*		ClassName				() { return "planars"; }
	IC const char*		ClassDesc				() { return "Planars"; }
	IC int				RenderPriority			() { return 11; }

	virtual bool		IsNeedSave				() { return true; }
	virtual bool		Export					(const char* path);

	virtual bool		Validate				(bool full_build);

	virtual void		BeforeRender			();
	virtual void		OnRender				(int priority, bool strictB2F);
	virtual void		AfterRender				();

	virtual void		FillProp				(const char* pref, PropItemVec& items);

	virtual CCustomObject* CreateObject			(LPVOID data, const char* name);
};
