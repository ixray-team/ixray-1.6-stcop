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
	if (!IsOpen)
		return;

	int uniqueId = 1;
	if (ImGui::Begin("Bones Node View", &IsOpen))
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
	BonesData.clear();

	for (auto Node : Nodes)
	{
		xr_delete(Node);
	}
	
	Nodes.clear();
	Links.clear();

	if (List.empty())
		return;

	xr_vector <CNodeShape*> ShapeNodeList;

	for (CBone* Bone : List)
	{
		CNodeBone* BoneNode = (CNodeBone*)Nodes.emplace_back(new CNodeBone(Bone->name.c_str()));
		BoneNode->SetBone(Bone);

		BonesData[Bone] = BoneNode;

		CNodeShape* ShapeNode = nullptr;
		auto& Shape = Bone->shape;

		if (Shape.type == SBoneShape::EShapeType::stNone)
			continue;

		for (auto ShapePtr : ShapeNodeList)
		{
			if (*ShapePtr == Shape)
			{
				ShapeNode = ShapePtr;
				break;
			}
		}

		if (ShapeNode == nullptr)
		{
			ShapeNode = (CNodeShape*)*Nodes.emplace(Nodes.begin(), new CNodeShape());
			ShapeNode->SetShape(&Shape);

			ShapeNodeList.push_back(ShapeNode);
		}

		BoneNode->AddChild(ShapeNode, ELinkType::eShape);

	}

	constexpr float NodeOffsetX = 200;
	constexpr float NodeOffsetY = 300;
	
	// Find root bone
	CBone* RootBone = *List.begin();
	while (RootBone->Parent() != nullptr)
	{
		RootBone = RootBone->Parent();
	}

	// Create link bone
	for (auto& [Bone, Node] : BonesData)
	{
		if (Bone->Parent())
		{
			int ParentID = BonesData[Bone->Parent()]->GetContactLink(true);
			int ContackID = Node->GetContactLink();

			Node->CreateContactLink(ParentID, ContackID);
		}
	}

	IterateChild(RootBone, { 350, 0 });
}

void CUIBoneView::Show(bool State)
{
	IsOpen = State;
}

float CUIBoneView::IterateChild(CBone* Bone, Fvector2 Offset)
{
	float NodeOffsetXIterator = Offset.x;
	float NodeOffsetYIterator = Offset.y;

	for (CBone* Child : Bone->children)
	{
		BonesData[Child]->SetStartPos(NodeOffsetXIterator, NodeOffsetYIterator);

		float ChildY = NodeOffsetYIterator + 150;
		for (INodeUnknown* ChildNodePtr : BonesData[Child]->Childs)
		{
			ChildNodePtr->SetStartPos(NodeOffsetXIterator - 150, ChildY);
			ChildY += 150;
		}

		if (Child->children.empty())
		{
			NodeOffsetYIterator += 350;
		}
		else
		{
			NodeOffsetYIterator = IterateChild(Child, { NodeOffsetXIterator + 350, NodeOffsetYIterator });
		}
	}

	return NodeOffsetYIterator;
}
