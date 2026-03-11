#include "stdafx.h"
UISectorTool::UISectorTool()
{
	m_Edit = false;
    m_CreateNewMultiple = false;
    m_CreateNewSingle = false;
    m_MeshAdd = true;
    m_BoxPick = false;
}

UISectorTool::~UISectorTool()
{
}
void UISectorTool::Draw()
{
	const float	ItemSpacingX = ImGui::GetStyle().ItemSpacing.x;

	if (XRay::ImGui::BeginDarkChild("ObjectToolsBorder", { 0, 0 }, ImGuiChildFlags_AutoResizeY))
	{
		ImGui::PushStyleVar(ImGuiStyleVar_IndentSpacing, 0.f);
		ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
		if (XRay::ImGui::BeginExpand("Commands"))
		{
			float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX) / 2;

			if (XRay::ImGui::Button("Validate Sectors", { SizeX, 0 })) PortalUtils.Validate(true);
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Capture Volume", { SizeX, 0 }))
			{
				CSector* S = PortalUtils.GetSelectedSector();
				if (S) {
					S->CaptureInsideVolume();
					Scene->UndoSave();
				}
			}

			//XRay::ImGui::Separator();
			if (XRay::ImGui::Button("Recalculate Portals", { SizeX, 0 })) {
				int Size = PortalUtils.CalculateAllPortals();
				if (Size > 0) {
					ELog.DlgMsg(mtInformation, "Recalculated %d portals.", Size);
				} else {
					ELog.DlgMsg(mtInformation, "Recalculated portals error! Portals is empty...");
				}
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Distribute Objects", { SizeX, 0 })) {
				CSector* S = PortalUtils.GetSelectedSector();
				if (S) {
					S->DistributeInsideObjects();
					Scene->UndoSave();
				}
			}

			XRay::ImGui::Separator();
			if (XRay::ImGui::Button("Create Default", { SizeX, 0 })) {
				CCustomObject* O = Scene->FindObjectByName(DEFAULT_SECTOR_NAME, OBJCLASS_SECTOR);
				if (O) {
					ELog.DlgMsg(mtInformation, "Default sector already present. Remove this and try again.");
				} else {
					if (!PortalUtils.CreateDefaultSector()) {
						ELog.DlgMsg(mtInformation, "Default can't created.");
					}
				}
			}
			ImGui::SameLine(0, ItemSpacingX);
			if (XRay::ImGui::Button("Remove Default", { SizeX, 0 })) {
				if (!PortalUtils.RemoveDefaultSector()) {
					ELog.DlgMsg(mtInformation, "Default sector not found.");
				}
			}

			XRay::ImGui::EndExpand();
		}
		if (m_Edit)
		{
			ImGui::SetNextItemOpen(true, ImGuiCond_FirstUseEver);
			if (XRay::ImGui::BeginExpand("Edit"))
			{
				float SizeX = (ImGui::GetContentRegionAvail().x - ItemSpacingX * 2) / 3;

				if (XRay::ImGui::ToggleButton("Create New Single (From Mesh)", &m_CreateNewSingle, { -0.01, 0 }))			m_CreateNewMultiple = false;
				if (XRay::ImGui::ToggleButton("Create New Multiple (From Object)", &m_CreateNewMultiple, { -0.01, 0 }))		m_CreateNewSingle = false;

				XRay::ImGui::Separator();

				XRay::ImGui::TextFramed("Meshes");
				ImGui::SameLine();
				bool MeshAdd = m_MeshAdd;
				bool MeshExclude = !m_MeshAdd;
				if (XRay::ImGui::ToggleButton("M+", MeshAdd))		m_MeshAdd = true;
				ImGui::SameLine();
				if (XRay::ImGui::ToggleButton("M-", MeshExclude))	m_MeshAdd = false;
				if (XRay::ImGui::ToggleButton("Box Pick", &m_BoxPick, { -0.01, 0 })) {
					if (m_CreateNewSingle || m_CreateNewMultiple)
						m_BoxPick = false;
				}

				XRay::ImGui::EndExpand();
			}
		}
		ImGui::PopStyleVar(); // IndentSpacing

		XRay::ImGui::EndDarkChild();
	}
}
