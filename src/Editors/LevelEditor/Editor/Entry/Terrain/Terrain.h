#pragma once
#include "Editor/Terrain/HeightmapUtils.h"

class CTerrain :
	public CCustomObject
{
	typedef CCustomObject inherited;
	friend class ESceneTerrainTool;

private:
	XRay::Editor::HeightmapUtils::SHeightMap HMap;
	bool IsPreview = false;
	int ScaleY = 50.f;
	int m_AppliedHMScale = 50;
	Fbox m_TBBox;

	// Terrain surface properties are owned by CTerrain (the source of truth)
	// and passed into the generated CEditableObject on each mesh rebuild.
	shared_str SurfaceShader;
	shared_str SurfaceShaderXRLC;
	shared_str SurfaceGameMtl;
	shared_str SurfaceTexture;

public:
	CTerrain(LPVOID data, const char* name);
	virtual ~CTerrain();

	void Construct(LPVOID data);

	virtual bool CanAttach() override { return false; }
	virtual bool LoadStream(IReader&) override;
	virtual void SaveStream(IWriter&) override;

	void InitializeHeightmap(u32 w, u32 h, float fill);
	void RebuildMesh();
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
};