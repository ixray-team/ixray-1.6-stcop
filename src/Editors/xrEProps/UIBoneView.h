#pragma once
#include "NodeBuilder/Builder.h"

class XREPROPS_API CUIBoneView:
	public CNodeViewport
{
	xr_hash_map<CBone*, CNodeBone*> BonesData;

	bool IsOpen = false;

public:
	CUIBoneView();
	~CUIBoneView();

	virtual void Draw() override;
	void FillBones(const BoneVec& List);
	void Show(bool State);

private:
	float IterateChild(CBone* Bone, Fvector2 Offset);
};