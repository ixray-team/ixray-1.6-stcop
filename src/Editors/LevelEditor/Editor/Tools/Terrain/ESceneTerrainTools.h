#pragma once
#include "../../Entry/Terrain/Terrain.h"

class ESceneTerrainTool: 
	public ESceneCustomOTool
{
	typedef ESceneCustomOTool inherited;
	friend class SceneBuilder;

protected:
	// light control
	int					lcontrol_last_idx;
	RTokenVec			lcontrols;
	void   	OnControlAppendClick		(ButtonValue* sender, bool& bDataModified, bool& bSafe);
	void   	OnControlRenameRemoveClick	(ButtonValue* sender, bool& bDataModified, bool& bSafe);
protected:

	// controls
	virtual void 		CreateControls			();
	virtual void 		RemoveControls			();
public:
	ESceneTerrainTool();
	virtual ~ESceneTerrainTool();

	virtual void		Clear					(bool bSpecific=false);

	// definition
	IC LPCSTR			ClassName				(){return "terrain";}
	IC LPCSTR			ClassDesc				(){return "terrain";}
	IC int				RenderPriority			(){return 1;}

	// IO
	virtual bool   		IsNeedSave				(){return true;}
	virtual BOOL 		_AppendObject(CCustomObject* object);

	// utils
	virtual bool		Validate				(bool full_build);
	
	virtual void		BeforeRender			();
	virtual void		OnRender				(int priority, bool strictB2F);
	virtual void		AfterRender				();
	virtual bool		can_use_inifile			() override { return false; }

	virtual void 		FillProp				(LPCSTR pref, PropItemVec& items);

	virtual CCustomObject* CreateObject			(LPVOID data, LPCSTR name);

private:
	virtual void OnDrawUI();
	virtual void GetStaticDesc(int& v_cnt, int& f_cnt, bool b_selected_only, bool b_cform) override;
};