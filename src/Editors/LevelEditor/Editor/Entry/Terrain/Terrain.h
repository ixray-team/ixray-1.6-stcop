#pragma once
#include "Editor/Terrain/HeightmapUtils.h"

class CTerrain :
	public CCustomObject
{
	typedef CCustomObject inherited;
	friend class ESceneTerrainTool;

private:
	XRay::Editor::HeightmapUtils::SHeightMap HMap;
	CEditableObject* TerrainObject;
	bool IsPreview = false;
	int ScaleY = 100.f;
	Fbox m_TBBox;

public:
	CTerrain(LPVOID data, LPCSTR name);
	virtual ~CTerrain();

	void Construct(LPVOID data);

	virtual bool CanAttach() override { return false; }
	virtual bool LoadStream(IReader&) override;
	virtual void SaveStream(IWriter&) override;
	virtual void OnFrame() override;
	virtual bool RayPick(float& dist, const Fvector& S, const Fvector& D, SRayPickInfo* pinf) override;
	virtual void Render(int priority, bool strictB2F) override;

	virtual void Move(Fvector& amount);
	virtual void Scale(Fvector& amount);
	virtual void OnUpdateTransform() override;

	virtual void FillProp(LPCSTR pref, PropItemVec& items) override;

	virtual void BoxQuery(SPickQuery& pinf) override;
	virtual void RayQuery(SPickQuery& pinf) override;


	virtual bool GetBox(Fbox& box) override;
	virtual bool GetUTBox(Fbox& box) override;
	
	IC CEditableObject* GetReference()
	{
		return TerrainObject;
	}
	bool BoxPick(const Fbox& box, SBoxPickInfoVec& pinf);

private:
	void OnChangeShader(PropValue* sender);
	void OnChangeSurface(PropValue* sender);

	bool OnChangeHMData(PropValue* sender, int& NewValue);
};