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
		bmNoise,
		bmHole,
		bmPaint,
		bmCount
	};

	static const char* BrushModeName(ETerrainBrushMode Mode);

	float				m_BrushSize;		// Brush radius (world units)
	float				m_BrushStrength;	// Normalized height delta per stamp at the brush center
	float				m_BrushHardness;	// 0 = soft falloff, 1 = hard edge
	float				m_BrushSpacing;		// Stamp spacing along the stroke, fraction of the radius
	float				m_NoiseScale;		// Noise feature size (world units)
	ETerrainBrushMode	m_BrushMode;
	bool				m_BrushInvert;		// Invert the brush (same as holding Ctrl)
	bool				m_FlattenPickOnStroke; // Take flatten target from the stroke start point
	float				m_FlattenTarget;	// Normalized target height for Flatten / hole restore
	u32					m_PaintLayer;		// Material mask channel painted by bmPaint (0..3 = R, G, B, A)
	bool				m_PaintMask;		// bmPaint writes the mask channel
	bool				m_PaintSurface;		// bmPaint writes the surface (color map)
	int					m_SurfaceSource;	// 0 = paint layer texture (tiled), 1 = solid color
	Fcolor				m_SurfaceColor;

	Fvector				m_BrushPos;			// Brush position for the overlay
	bool				m_BrushActive;		// Overlay visible

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
	CTerrain*			m_HoverTerrain;
	Fvector				m_LastStampPos;
	bool				m_HasLastStamp;
	bool				m_StrokeInvert;
	float				m_StrokePlaneY;		// World height of the stroke start (hole strokes stay on this plane)
	xr_vector<float>	m_SmoothScratch;

	CTerrain*			PickTerrain				(float& dist, Fvector& point);
	void				BeginSculpt				(CTerrain* obj, const Fvector& worldPoint, bool invert);
	void				StrokeTo				(CTerrain* obj, const Fvector& worldPoint, bool invert);
	void				EndSculpt				();
	void				PickFlattenTarget		(CTerrain* obj, const Fvector& worldPoint);
	void				SculptTerrain			(CTerrain* obj, const Fvector& worldPoint, bool invert);
	float				BrushFalloff			(float distance) const;
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

	// Create an empty heightmap plane
	void				CreateTerrain			(LPCSTR name, u32 w, u32 h, float fill);

	// Whole-map operations on the selected (or the only) terrain
	CTerrain*			GetActiveTerrain		();
	CTerrain*			GetHoverTerrain			() const { return m_HoverTerrain; }
	void				OpFill					(CTerrain* obj, float height);
	void				OpSmooth				(CTerrain* obj, int passes);
	void				OpNormalize				(CTerrain* obj);
	void				OpInvert				(CTerrain* obj);
	void				OpFillHoles				(CTerrain* obj);
	void				OpResize				(CTerrain* obj, u32 size);
	bool				OpImportR16				(CTerrain* obj, const char* fileName);
	bool				OpExportR16				(CTerrain* obj, const char* fileName);
	void				OpApplyMesh				(CTerrain* obj);
	void				OpClearPaint			(CTerrain* obj);
	void				OpClearSurfacePaint		(CTerrain* obj);
	bool				OpBakeMask				(CTerrain* obj, u32 size);

private:
	void				FinishOp				(CTerrain* obj);
	virtual void OnDrawUI();
	virtual void GetStaticDesc(int& v_cnt, int& f_cnt, bool b_selected_only, bool b_cform) override;
};
