#pragma once
#include "NodeBuilder/Builder.h"

class XREPROPS_API CUIBoneView:
	public CNodeViewport
{
public:
	CUIBoneView();
	~CUIBoneView();

	virtual void Draw() override;
	void FillBones(const BoneVec& List);
};