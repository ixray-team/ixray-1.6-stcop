#pragma once
#include "../../Entry/Road/Road.h"

class ESceneRoadTool :
	public ESceneCustomOTool
{
	typedef ESceneCustomOTool inherited;
	friend class SceneBuilder;
	friend class TUI_ControlRoadEdit;

public:
	enum ERoadSubTarget
	{
		estRoadEdit = 1,
	};

	float m_DefaultWidth = 6.f; // Width of newly added points

protected:
	virtual void CreateControls();
	virtual void RemoveControls();

public:
	ESceneRoadTool();
	virtual ~ESceneRoadTool();

	IC const char* ClassName() { return "road"; }
	IC const char* ClassDesc() { return "Road"; }
	IC int RenderPriority() { return 2; }

	virtual void Clear(bool bSpecific = false) override { inherited::Clear(bSpecific); }
	virtual bool IsNeedSave() { return true; }
	virtual bool can_use_inifile() override { return false; }
	virtual bool Validate(bool full_build) { return true; }

	virtual void BeforeRender() {}
	virtual void OnRender(int priority, bool strictB2F);
	virtual void AfterRender() {}
	virtual void OnDeactivate();

	virtual CCustomObject* CreateObject(LPVOID data, const char* name);

	CRoad* GetActiveRoad();
	CRoad* CreateRoad(const char* name);

	void SnapPointsToTerrain(CRoad* Road);
	void ConformTerrain(CRoad* Road);
	void DeletePoint(CRoad* Road, int Index);

private:
	virtual void GetStaticDesc(int& v_cnt, int& f_cnt, bool b_selected_only, bool b_cform) override;
};
