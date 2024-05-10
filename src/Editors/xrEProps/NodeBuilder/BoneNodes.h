#pragma once
#include "Nodes.h"

class CNodeBone:
	public INodeUnknown
{
	CBone* BonePtr = nullptr;

	xr_string BoneName;
	float Mass = 0;

	Fvector3 MassPos = {};
	Fvector3 Pos = {};
	Fvector3 Rot = {};

public:
	CNodeBone(const char* Name);
	void Draw() override;

	void SetBone(CBone* Ptr);
};

class CNodeShape :
	public INodeUnknown
{
	bool Pickable = true;
	bool Physics = true;
	bool RemoveAfterBreak = false;
	bool FogCollider = true;
	int Type = SBoneShape::stNone;

public:
	CNodeShape();
	void Draw() override;
};