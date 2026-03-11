#include "stdafx.h"

UILightTool::UILightTool()
{
}

UILightTool::~UILightTool()
{
}

void UILightTool::Draw()
{
	float ItemSpacingX	= ImGui::GetStyle().ItemSpacing.x;

	if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Affect in D3D"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;
			if (XRay::ImGui::Button("Enable Sel", { SizeX, 0 })) UseInD3D(false, true);
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Enable All", { SizeX, 0 })) UseInD3D(true, true);

			if (XRay::ImGui::Button("Disable Sel", { SizeX, 0 })) UseInD3D(false, false);
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Disable All", { SizeX, 0 })) UseInD3D(true, false);

			XRay::ImGui::EndExpand();
		}

		ImGui::PopStyleVar(); // IndentSpacing

		XRay::ImGui::EndDarkChild();
	}
}

void UILightTool::UseInD3D(bool bAll, bool bFlag)
{
	ObjectIt _F = Scene->FirstObj(OBJCLASS_LIGHT);
	ObjectIt _E = Scene->LastObj(OBJCLASS_LIGHT);
	for (; _F != _E; _F++) {
		CLight* L = (CLight*)*_F;
		if (bAll) {
			L->AffectD3D(bFlag);
		}
		else {
			if (L->Selected() && L->Visible()) L->AffectD3D(bFlag);
		}
	}
	UI->RedrawScene();
}
