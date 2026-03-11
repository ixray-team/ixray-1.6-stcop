#include "stdafx.h"
UIPortalTool::UIPortalTool()
{
}

UIPortalTool::~UIPortalTool()
{
}

void UIPortalTool::Draw()
{
	if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);

		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Command"))
		{
			if (XRay::ImGui::Button("Invert Orientation", ImVec2(-0.01, 0)))
			{
				ObjectList lst;
				if (Scene->GetQueryObjects(lst, OBJCLASS_PORTAL, 1, 1, 0)) {
					for (ObjectIt it = lst.begin(); it != lst.end(); it++) {
						CPortal* _O = (CPortal*)*it;
						_O->InvertOrientation(true);
					}
				}
			}
			if (XRay::ImGui::Button("Compute All Portals", ImVec2(-0.01, 0)))
			{
				if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure want to destroy all existing portals and compute them again?"))
				{
					int cnt = PortalUtils.CalculateAllPortals();
					if (cnt) ELog.DlgMsg(mtInformation, "Calculated '%d' portal(s).", cnt);
				}
			}
			if (XRay::ImGui::Button("Compute Sel. Portals", ImVec2(-0.01, 0)))
			{
				if (mrYes == ELog.DlgMsg(mtConfirmation, mbYes | mbNo, "Are you sure want to destroy all existing portals and compute them again?"))
				{
					int cnt = PortalUtils.CalculateSelectedPortals();
					if (cnt) ELog.DlgMsg(mtInformation, "Calculated '%d' portal(s).", cnt);
				}
			}
			if (XRay::ImGui::Button("Remove Similar", ImVec2(-0.01, 0)))
			{
				tool->RemoveSimilar();
			}
			XRay::ImGui::EndExpand();
		}

		ImGui::PopStyleVar(); // IndentSpacing

		XRay::ImGui::EndDarkChild();
	}
}
