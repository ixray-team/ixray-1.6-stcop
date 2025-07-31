#pragma once
#include "Editor/Terrain/HeightmapUtils.h"

class CTerrain:
	public CCustomObject
{
	typedef CCustomObject inherited;

private:
	XRay::Editor::HeightmapUtils::SHeightMap HMap;
	CEditableObject TerrainObject;

public:
	CTerrain(LPVOID data, LPCSTR name);
	virtual ~CTerrain();

	void 			Construct	(LPVOID data);

	virtual bool	CanAttach() { return false; }
  	virtual bool 	LoadStream			(IReader&);
	virtual void 	SaveStream			(IWriter&);
	virtual void	OnFrame				() override;
	virtual void 	Render(int priority, bool strictB2F);

	virtual void 	Move(Fvector& amount);
	virtual void 	Scale(Fvector& amount);

	virtual void 	OnUpdateTransform() override;
};