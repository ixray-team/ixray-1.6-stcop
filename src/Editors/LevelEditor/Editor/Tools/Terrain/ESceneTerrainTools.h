#pragma once
#include "../../Entry/Terrain/Terrain.h"

class UITerrainTool;

class ESceneTerrainTool: 
	public ESceneCustomOTool
{
	typedef ESceneCustomOTool inherited;
	friend class SceneBuilder;
	friend class TUI_ControlTerrainSculpt;

public:
	enum ETerrainSubTarget
	{
		estTerrainSculpt = 1,
	};

	enum ETerrainBrushMode
	{
		bmRaise,
		bmLower,
		bmSmooth,
		bmFlatten,
	};

	int					m_BrushSize;		// радиус кисти (в мировых единицах)
	float				m_BrushStrength;	// сила воздействия кисти
	ETerrainBrushMode	m_BrushMode;		// режим кисти
	Fvector				m_BrushPos;			// позиция кисти (для оверлея)
	bool				m_BrushActive;		// активен ли оверлей кисти

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

	// sculpt
	CTerrain*			m_EditedTerrain;
	float				m_FlattenTarget;
	CTerrain*			PickTerrain				(float& dist, Fvector& point);
	void				BeginSculpt				(CTerrain* obj, const Fvector& worldPoint);
	void				SculptTerrain			(CTerrain* obj, const Fvector& worldPoint);
	void				RenderBrush				();

public:
	ESceneTerrainTool();
	virtual ~ESceneTerrainTool();

	virtual void		Clear					(bool bSpecific=false);

	virtual void		OnDeactivate				();

	// definition
	IC const char*			ClassName				(){return "terrain";}
	IC const char*			ClassDesc				(){return "Terrain";}
	IC int				RenderPriority			(){return 1;}

	// IO
	virtual bool   		IsNeedSave				(){return true;}
	virtual bool 		_AppendObject(CCustomObject* object);

	// utils
	virtual bool		Validate				(bool full_build);
	
	virtual void		BeforeRender			();
	virtual void		OnRender				(int priority, bool strictB2F);
	virtual void		AfterRender				();
	virtual bool		can_use_inifile			() override { return false; }

	virtual void 		FillProp				(const char* pref, PropItemVec& items);

	virtual CCustomObject* CreateObject			(LPVOID data, const char* name);

	// создание пустой плоскости высот
	void				CreateTerrain			(LPCSTR name, u32 w, u32 h, float fill);

private:
	virtual void OnDrawUI();
	virtual void GetStaticDesc(int& v_cnt, int& f_cnt, bool b_selected_only, bool b_cform) override;
};
