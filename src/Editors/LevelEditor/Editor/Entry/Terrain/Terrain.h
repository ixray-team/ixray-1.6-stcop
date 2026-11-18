#pragma once
#include "Editor/Terrain/HeightmapUtils.h"
#include "Editor/Terrain/TerrainMask.h"

class CTerrain :
	public CCustomObject
{
	typedef CCustomObject inherited;
	friend class ESceneTerrainTool;

private:
	XRay::Editor::HeightmapUtils::SHeightMap HMap;
	bool IsPreview = false;
	shared_str m_PreviewTexLoaded;   // Surface texture currently sampled into the heightmap preview
	bool m_PreviewTexTried = false;
	int ScaleY = 50;
	int m_AppliedHMScale = 50;
	Fbox m_TBBox;

	// Terrain surface properties are owned by CTerrain (the source of truth)
	// and passed into the generated CEditableObject on each mesh rebuild.
	shared_str SurfaceShader;
	shared_str SurfaceShaderXRLC;
	shared_str SurfaceGameMtl;
	shared_str SurfaceTexture;

	// Material mask (4 detail layers), baked into <texture>_mask
	XRay::Editor::TerrainMask::CTerrainMask Mask;

public:
	CTerrain(LPVOID data, const char* name);
	virtual ~CTerrain();

	void Construct(LPVOID data);

	virtual bool CanAttach() override { return false; }
	virtual bool LoadStream(IReader&) override;
	virtual void SaveStream(IWriter&) override;

	void InitializeHeightmap(u32 w, u32 h, float fill);
	void RebuildMesh();

	XRay::Editor::HeightmapUtils::SHeightMap& GetHeightmap() { return HMap; }
	float GetScaleY() const { return float(ScaleY); }
	bool IsMeshPreview() const { return IsPreview; }

	// Heightmap preview: height tint / surface texture / material mask
	u32 GetPreviewMode() const { return u32(HMap.PreviewMode); }
	void SetPreviewMode(u32 Mode);
	void ReloadPreviewTexture();

	// Material mask
	XRay::Editor::TerrainMask::CTerrainMask& GetMask() { return Mask; }
	void EnsureMask();                                   // Rebuild whatever is dirty
	void MarkMaskDirty();
	void OnMaskPainted(int x0, int z0, int x1, int z1);
	static void OnRoadsChanged();                        // Roads moved / edited: their footprint must be re-rasterized
	bool BakeMask(u32 Size, xr_string& OutColor, xr_string& OutMask);
	xr_string MaskBaseName() const;

	// Ray pick against the heightmap itself (not the generated mesh, which may be stale while sculpting)
	bool PickHeightmap(const Fvector& Start, const Fvector& Dir, float& Dist, Fvector& WorldPoint);
	// Called after the heightmap data was modified in the grid rect [x0, x1] x [z0, z1]
	void OnHeightmapChanged(int x0, int z0, int x1, int z1);
	void OnHeightmapChanged();
	XRay::Editor::HeightmapUtils::STerrainSurfaceTemplate SurfaceTemplate() const;
	virtual void OnFrame() override;
	virtual bool RayPick(float& dist, const Fvector& S, const Fvector& D, SRayPickInfo* pinf) override;
	virtual void Render(int priority, bool strictB2F) override;

	virtual void Move(Fvector& amount);
	virtual void Scale(Fvector& amount);
	virtual void OnUpdateTransform() override;

	virtual void OnDeviceDestroy() override;

	virtual void FillProp(const char* pref, PropItemVec& items) override;

	virtual void BoxQuery(SPickQuery& pinf) override;
	virtual void RayQuery(SPickQuery& pinf) override;


	virtual bool GetBox(Fbox& box) override;
	virtual bool GetUTBox(Fbox& box) override;
	
	IC CEditableObject* GetReference()
	{
		return TerrainObject;
	}
	bool BoxPick(const Fbox& box, SBoxPickInfoVec& pinf);

	CEditableObject* TerrainObject;

private:
	void OnChangeSurfaceProp(PropValue* sender);
	void ApplySurfaceTemplate();

	bool OnChangeHMData(PropValue* sender, int& NewValue);
	void OnChangePreview(PropValue* sender);
	void OnChangeTexturePreview(PropValue* sender);
	void OnChangeMaskProp(PropValue* sender);
	void RebuildRoadMask();
	void UpdateMaskPreview(int x0, int z0, int x1, int z1);
	Fvector CellWorld(int x, int z);
	void UpdatePreviewTexture();
};