#include "stdafx.h"

UIShapeTool::UIShapeTool()
{
    Tool = nullptr;
    m_AttachShape = false;
}

UIShapeTool::~UIShapeTool()
{
}

void UIShapeTool::Draw()
{
	const   float   ItemSpacingX = ImGui::GetStyle().ItemSpacing.x;
	bool    ModeSphere = m_SphereMode;
	bool    ModeBox = !m_SphereMode;

	if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Commands"))
		{
			if (XRay::ImGui::ToggleButton("Sphere", &ModeSphere))	m_SphereMode = true;
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::ToggleButton("Box", &ModeBox))			m_SphereMode = false;

			XRay::ImGui::EndExpand();
		}
		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Edit"))
		{
			const float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;
			if (XRay::ImGui::ToggleButton("Attach Shape...", &m_AttachShape, { SizeX, 0 })) {
				if (m_AttachShape)
					ExecCommand(COMMAND_CHANGE_ACTION, etaAdd);
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (ImGui::Button("Detach All", { SizeX, 0 })) {
				ObjectList lst;
				if (Scene->GetQueryObjects(lst, OBJCLASS_SHAPE, 1, 1, 0)) {
					Scene->SelectObjects(false, OBJCLASS_SHAPE);
					for (ObjectIt it = lst.begin(); it != lst.end(); it++)
						((CEditShape*)*it)->Detach();
				}
			}

			XRay::ImGui::EndExpand();
		}
		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Level Bound"))
		{
			if (XRay::ImGui::ToggleButton("Edit Level Bound", &EditLevelBound, { -0.01, 0 })) {
				if (EditLevelBound)
					Tool->OnEditLevelBounds(false);
			}
			if (EditLevelBound) {
				if (ImGui::Button("Recalc", { -1, 0 }))
					Tool->OnEditLevelBounds(true);
			}

			XRay::ImGui::EndExpand();
		}

		ImGui::PopStyleVar(); // IndentSpacing

		XRay::ImGui::EndDarkChild();
	}
}
