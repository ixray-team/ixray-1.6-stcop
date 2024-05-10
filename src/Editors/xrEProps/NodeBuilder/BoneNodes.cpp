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
	ImGui::Text("Pick mode:");
	ImGui::SameLine();
	ImGui::PushItemWidth(60);
	ImGui::Combo("##", &Type, PickModeList, 4, -1);
	ImGui::PopItemWidth();

	DrawEnd();
}
