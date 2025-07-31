#pragma once
#include "Editor/Terrain/HeightmapUtils.h"

class CTerrain :
	public CCustomObject
{
	typedef CCustomObject inherited;

private:
	XRay::Editor::HeightmapUtils::SHeightMap HMap;
	CEditableObject TerrainObject;

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
};