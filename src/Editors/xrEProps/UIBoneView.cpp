#include "stdafx.h"
#include "UIBoneView.h"
#include "../xrEUI/ImNodeEditor/imnodes.h"

CUIBoneView::CUIBoneView()
{
}

CUIBoneView::~CUIBoneView()
{
	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}
}

void CUIBoneView::Draw()
{
	int uniqueId = 1;
	if (ImGui::Begin("Bones Node View"))
	{
		auto& io = ImGui::GetIO();
		ImGui::Text("FPS: %.2f (%.2gms)", io.Framerate, io.Framerate ? 1000.0f / io.Framerate : 0.0f);

		ImGui::Separator();

		CNodeViewport::Draw();
	}

	ImGui::End();

	CNodeViewport::DrawEnd();
}

void CUIBoneView::FillBones(const BoneVec& List)
{
	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}

	xr_hash_map<CBone*, CNodeBone*> BonesData;

	for (CBone* Bone : List)
	{
		CNodeBone* BoneNode = (CNodeBone*)Nodes.emplace_back(new CNodeBone(Bone->name.c_str()));
		BoneNode->SetBone(Bone);

		BonesData[Bone] = BoneNode;
	}

	for (auto& [Bone, Node] : BonesData)
	{
		if (Bone->Parent())
		{
			int ParentID = BonesData[Bone->Parent()]->GetContactLink(true);
			int ContackID = Node->GetContactLink();

			Links.emplace_back(ParentID, ContackID);
		}
	}
}
