#include "stdafx.h"
#include "BoneNodes.h"
#include "../xrEUI/ImNodeEditor/imnodes.h"

CNodeBone::CNodeBone(const char* Name) :
	INodeUnknown("Bone") 
{
	BoneName = Name;
	BoneName.resize(32);

	AddInLink("Shape", eShape, ImColor(111, 42, 132));
	AddInLink("Joint", eJoint, ImColor(11, 72, 122));

	AddContactLink("Out", true);
	AddContactLink("In");
};

void CNodeBone::Draw()
{
	INodeUnknown::Draw();

	DrawHeader();

	ImGui::Text("Name:");
	ImGui::SameLine();
	ImGui::PushItemWidth(120);
	ImGui::InputText("", BoneName.data(), 32);
	ImGui::PopItemWidth();

	ImGui::Text("Mass:");
	ImGui::SameLine();
	ImGui::PushItemWidth(60);
	ImGui::InputFloat("##", &Mass);
	ImGui::PopItemWidth();

	ImGui::Text("Mass center:");
	ImGui::SameLine();
	ImGui::PushItemWidth(100);
	ImGui::InputFloat3("##mass", &MassPos.x, "%.2f");
	ImGui::PopItemWidth();

	ImGui::Text("Position:");
	ImGui::SameLine();
	ImGui::PushItemWidth(120);
	ImGui::InputFloat3("##pos", &Pos.x, "%.2f");
	ImGui::PopItemWidth();

	ImGui::Text("Rotation:");
	ImGui::SameLine();
	ImGui::PushItemWidth(120);
	ImGui::InputFloat3("##rot", &Rot.x, "%.2f");
	ImGui::PopItemWidth();

	DrawEnd();
}

void CNodeBone::SetBone(CBone* Ptr)
{
	BonePtr = Ptr;

	Mass = BonePtr->mass;
	Rot = BonePtr->_RestRotate();
	Pos = BonePtr->_RestOffset();
	MassPos = BonePtr->center_of_mass;
}

CNodeShape::CNodeShape() :
	INodeUnknown("Shape")
{
	Header = { 122, 42, 122 };

	AddOutLink("Out", eShape, ImColor(111, 42, 132));
}

void CNodeShape::Draw()
{
	INodeUnknown::Draw();

	DrawHeader();

	ImGui::Checkbox("Pickable", &Pickable);
	ImGui::Checkbox("Physics", &Physics);
	ImGui::Checkbox("Remove After Break", &RemoveAfterBreak);
	ImGui::Checkbox("Fog Collider", &FogCollider);

	static const char* PickModeList[] = { "None","Box","Shape", "Cylinder"};
	ImGui::Text("Shape type:");
	ImGui::SameLine();
	ImGui::PushItemWidth(60);
	ImGui::Combo("##", &Type, PickModeList, 4, -1);
	ImGui::PopItemWidth();

	if (ShapePtr != nullptr)
	{
		switch (Type)
		{
		case SBoneShape::stBox:
		{
			ImGui::Text("Rotate:");
			ImGui::PushItemWidth(150);
			ImGui::InputFloat3("##", &ShapePtr->box.m_rotate.i.x);
			ImGui::InputFloat3("##", &ShapePtr->box.m_rotate.j.x);
			ImGui::InputFloat3("##", &ShapePtr->box.m_rotate.k.x);
			ImGui::PopItemWidth();

			ImGui::Text("Translate:");
			ImGui::PushItemWidth(150);
			ImGui::InputFloat3("##", &ShapePtr->box.m_translate.x);
			ImGui::PopItemWidth();

			ImGui::Text("Size:");
			ImGui::SameLine();
			ImGui::PushItemWidth(120);
			ImGui::InputFloat3("##", &ShapePtr->box.m_halfsize.x);
			ImGui::PopItemWidth();
		}break;
		case SBoneShape::stSphere: 
		{
			ImGui::Text("Pos:");
			ImGui::SameLine();
			ImGui::PushItemWidth(130);
			ImGui::InputFloat3("##", &ShapePtr->sphere.P.x);
			ImGui::PopItemWidth();

			ImGui::Text("Radius:");
			ImGui::SameLine();
			ImGui::PushItemWidth(100);
			ImGui::InputFloat("##", &ShapePtr->sphere.R);
			ImGui::PopItemWidth();
		}break;
		case SBoneShape::stCylinder:
		{
			ImGui::Text("Center:");
			ImGui::SameLine();
			ImGui::PushItemWidth(120);
			ImGui::InputFloat3("##", &ShapePtr->cylinder.m_center.x);
			ImGui::PopItemWidth();

			ImGui::Text("Dir:");
			ImGui::SameLine();
			ImGui::PushItemWidth(130);
			ImGui::InputFloat3("##", &ShapePtr->cylinder.m_direction.x);
			ImGui::PopItemWidth();

			ImGui::Text("Height:");
			ImGui::SameLine();
			ImGui::PushItemWidth(100);
			ImGui::InputFloat("##", &ShapePtr->cylinder.m_height);
			ImGui::PopItemWidth();

			ImGui::Text("Radius:");
			ImGui::SameLine();
			ImGui::PushItemWidth(100);
			ImGui::InputFloat("##", &ShapePtr->cylinder.m_radius);
			ImGui::PopItemWidth();
		}break;
		}
	}

	DrawEnd();
}

void CNodeShape::SetShape(SBoneShape* Ptr)
{
	ShapePtr = Ptr;

	Pickable = !Ptr->flags.test(SBoneShape::sfNoPickable);
	Physics = !Ptr->flags.test(SBoneShape::sfNoPhysics);
	RemoveAfterBreak = Ptr->flags.test(SBoneShape::sfRemoveAfterBreak);
	FogCollider = !Ptr->flags.test(SBoneShape::sfNoFogCollider);
	Type = Ptr->type;
}